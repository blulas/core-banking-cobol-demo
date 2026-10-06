      *================================================================*
      * PROGRAM  : ATNPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ATM NETWORK SETTLEMENT - PURGE AND ARCHIVE. READS   *
      *            THE ATN TRANSACTIONS, UPDATES THE ATN MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. PROPRIETARY SHARED ATM *
      *            NETWORK SETTLEMENT AND INTERCHANGE                  *
      * INPUT    : ATNMST (KSDS), ATNTRN, BANKCTL                      *
      * OUTPUT   : ATNOUT, ATNPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ATVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-27 DPS  ORIGINAL                                       *
      * 2000-01-18 SKN  NEW SURCHG REPORT COLUMNS PRB9224              *
      * 2000-04-17 CRP  AUDIT FINDING - DISPENSED CONTROL TOTALS CHG428*
      * 2008-04-21 PQR  PERFORMANCE - DISPENSED READ AHEAD INC6167     *
      * 2010-11-19 BHS  NEW INTERCHG REPORT COLUMNS CHG8774            *
      * 2014-08-09 PQR  ADJUST STATUS CODES ADDED PRB3653              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATNPURG.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ATNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ATNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ATNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ATNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ATNPURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ATNMST.
       FD  TRAN-FILE.
       COPY ATNTRN.
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
      * WS-ADJUST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ADJUST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000231500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000502000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000859000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000508500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000701000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000526000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025100000000036500'.
       01  WS-ADJUST-GRID-TBL REDEFINES WS-ADJUST-GRID-VALUES.
           05  WS-ADJUST-GRID-ROW OCCURS 7 TIMES.
               10  WS-ADJUST-GRID-LOW      PIC 9(09)V99.
               10  WS-ADJUST-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SURCHG-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURCHG-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000157000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000130500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000303000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000051000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004250000000142500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004300000000457500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006800000000801000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006900000000095500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000460500'.
       01  WS-SURCHG-TIER-TBL REDEFINES WS-SURCHG-TIER-VALUES.
           05  WS-SURCHG-TIER-ROW OCCURS 9 TIMES.
               10  WS-SURCHG-TIER-LOW      PIC 9(09)V99.
               10  WS-SURCHG-TIER-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ATNPURG  ATM NETWORK SETTLEMENT'.
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
       COPY ATVALP.

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
               MOVE 'ATNMST' TO ABND-FILE-NAME
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
           MOVE ATNT-KEY TO ATNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-AGE-EFFECTIVE
               PERFORM 3010-ACCUM-NETWORK
               PERFORM 3020-AGE-MATURITY
               PERFORM 3030-ACCRUE-DISCOUNT
               PERFORM 3040-EDIT-OPERATOR
               PERFORM 3050-AGE-OPEN
               PERFORM 3060-LIMIT-CASSETTE
               PERFORM 3070-AGE-LAST-PAY
               PERFORM 3080-SELECT-RATE
               PERFORM 3090-SELECT-RATE
               PERFORM 3100-ACCUM-BRANCH
               PERFORM 3110-SET-TERMINAL
               PERFORM 3120-COUNT-PAYMENT
               PERFORM 3130-ACCRUE-ADJUST
               PERFORM 3140-AGE-EFFECTIVE
               PERFORM 3150-ACCRUE-DISPENSED
               PERFORM 3160-PRORATE-SURCHG
               PERFORM 3170-SPLIT-CASSETTE
               PERFORM 3180-LIMIT-INTERCHG
               PERFORM 3190-AGE-NEXT-DUE
               PERFORM 3200-SET-STATUS
               PERFORM 3210-ASSESS-STOP
               PERFORM 3220-CHECK-POST
               PERFORM 3230-LIMIT-DISPENSED
               PERFORM 3240-SELECT-RATE
               PERFORM 3250-PRORATE-DISPENSED
               PERFORM 3260-LIMIT-DISPENSED
               PERFORM 3270-EDIT-REJECT
               PERFORM 3280-SET-STATUS
               PERFORM 3290-PRORATE-CASSETTE
               PERFORM 3300-ACCRUE-SURCHG
               PERFORM 3310-ASSESS-STOP
               PERFORM 3320-AGE-CLOSE
               PERFORM 3330-COUNT-RETRY
               PERFORM 3340-ACCUM-CHANNEL
               PERFORM 3350-EDIT-CONTACT
               PERFORM 3360-EDIT-DESC
               PERFORM 3370-FLAG-ITEM
               PERFORM 3380-PRORATE-INTERCHG
               PERFORM 3390-COUNT-PAYMENT
               PERFORM 3400-ASSESS-REVIEW
               PERFORM 3410-CHECK-CLOSE
               PERFORM 3420-CHECK-NOTICE
               PERFORM 3430-ACCUM-REGION
               PERFORM 3440-PRORATE-SETTLE
               PERFORM 3450-ASSESS-HOLD
               PERFORM 3460-SET-STATUS
               PERFORM 3470-PRORATE-SETTLE
               PERFORM 3480-EDIT-CONTACT
               PERFORM 3490-PRORATE-DISPENSED
               PERFORM 3500-SPLIT-SURCHG
               PERFORM 3510-AGE-NEXT-REVIEW
               PERFORM 3520-SPLIT-ADJUST
               PERFORM 3530-ACCRUE-SETTLE
               PERFORM 3540-CHECK-OPEN
               PERFORM 3550-SPLIT-DISPENSED
               PERFORM 3560-ACCUM-TERMINAL
               PERFORM 3570-EDIT-CUST-ID
               PERFORM 3580-ACCRUE-SETTLE
               PERFORM 3590-COUNT-SEQ
               PERFORM 3600-AGE-POST
               PERFORM 3610-LIMIT-DISPENSED
               PERFORM 3620-SET-BRANCH
               PERFORM 3630-SPLIT-DISPENSED
               PERFORM 3640-PRORATE-SURCHG
               PERFORM 3650-EDIT-REJECT
               PERFORM 3660-ASSESS-STOP
               PERFORM 3670-DESCRIBE-CHANNEL
               PERFORM 3680-PRORATE-INTERCHG
               PERFORM 3690-FLAG-CASSETTE
               PERFORM 3700-SET-REGION
               PERFORM 3710-SPLIT-ADJUST
               PERFORM 3720-EDIT-NAME
               PERFORM 3730-EDIT-ADDR
               PERFORM 3740-LIMIT-SURCHG
               PERFORM 3750-FLAG-CASSETTE
               PERFORM 3760-CHECK-NOTICE
               PERFORM 3770-AGE-LAST-PAY
               PERFORM 3780-CHECK-EFF
               PERFORM 3790-ACCRUE-CASSETTE
               PERFORM 3800-ACCRUE-SETTLE
               PERFORM 3810-SELECT-DISCOUNT
               PERFORM 3820-SELECT-FEE-PCT
               PERFORM 3830-CHECK-LAST-PAY
               PERFORM 3840-ACCRUE-ADJUST
               PERFORM 3850-EDIT-ADDR
               PERFORM 3860-LIMIT-ITEM
               PERFORM 3870-COUNT-RETRY
               PERFORM 3880-ASSESS-STOP
               PERFORM 3890-SPLIT-ADJUST
               PERFORM 3900-CHECK-MATURITY
               PERFORM 3910-ASSESS-EXEMPT
               PERFORM 3920-ACCUM-TRAN
               PERFORM 3930-LIMIT-CASSETTE
               PERFORM 3940-EDIT-ADDR
               PERFORM 3950-DESCRIBE-REGION
               PERFORM 3960-ACCRUE-ADJUST
               PERFORM 3970-SPLIT-DISPENSED
               PERFORM 3980-SELECT-RATE
               PERFORM 3990-SELECT-FEE-PCT
               PERFORM 4000-CHECK-OPEN
               PERFORM 4010-ASSESS-REVERSAL
               PERFORM 4020-ACCUM-TERMINAL
               PERFORM 4030-ACCRUE-SURCHG
               PERFORM 4040-EDIT-CONTACT
               PERFORM 4050-SET-REASON
               PERFORM 4060-SPLIT-SURCHG
               PERFORM 4070-FLAG-CASSETTE
               PERFORM 4080-ACCUM-BRANCH
               PERFORM 4090-COUNT-ITEM
               PERFORM 4100-SELECT-FEE-PCT
               PERFORM 4110-ASSESS-HOLD
               PERFORM 4120-ACCRUE-SURCHG
               PERFORM 4130-EDIT-REJECT
               PERFORM 4140-COUNT-NOTICE
               PERFORM 4150-ACCRUE-SURCHG
               PERFORM 4160-EDIT-OPERATOR
               PERFORM 4170-AGE-EFF
               PERFORM 4180-EDIT-REF-NO
               PERFORM 4190-AGE-LAST-PAY
               PERFORM 4200-LIMIT-SURCHG
               PERFORM 4210-ACCRUE-ADJUST
               PERFORM 4220-DESCRIBE-TRAN
               PERFORM 4230-AGE-CLOSE
               PERFORM 4240-DESCRIBE-CHANNEL
               PERFORM 4250-CHECK-POST
               PERFORM 4260-COUNT-RETRY
               PERFORM 4270-SELECT-RATE
               PERFORM 4280-CHECK-NOTICE
               PERFORM 4290-EDIT-DESC
               PERFORM 4300-CHECK-NEXT-DUE
               PERFORM 4310-FLAG-SURCHG
               PERFORM 4320-ACCUM-REGION
               PERFORM 4330-AGE-OPEN
               PERFORM 4340-ACCRUE-DISPENSED
               PERFORM 4350-COUNT-SEQ
               PERFORM 4360-LIMIT-SURCHG
               PERFORM 4370-SET-REASON
               PERFORM 4380-CHECK-NEXT-DUE
               PERFORM 4390-EDIT-NAME
               PERFORM 4400-COUNT-PAYMENT
               PERFORM 4410-SPLIT-ADJUST
               PERFORM 4420-CHECK-NEXT-REVIEW
               PERFORM 4430-ASSESS-EXEMPT
               PERFORM 4440-ACCRUE-SURCHG
               PERFORM 4450-ACCUM-REGION
               PERFORM 4460-COUNT-ITEM
               PERFORM 4470-CHECK-EXPIRY
               PERFORM 4480-ACCUM-BRANCH
               PERFORM 4490-CHECK-OPEN
               PERFORM 4500-LIMIT-SURCHG
               PERFORM 4510-ACCUM-TRAN
               PERFORM 4520-COUNT-NOTICE
               PERFORM 4530-LIMIT-DISPENSED
               PERFORM 4540-CHECK-CLOSE
               PERFORM 4550-FLAG-INTERCHG
               PERFORM 4560-SPLIT-INTERCHG
               PERFORM 4570-ACCUM-REASON
               PERFORM 4580-PRORATE-SURCHG
               PERFORM 4590-ACCRUE-SURCHG
               PERFORM 4600-FLAG-DISPENSED
               PERFORM 4610-SPLIT-SURCHG
               PERFORM 4620-ASSESS-NOTICE-SENT
               PERFORM 4630-ASSESS-REVERSAL
               PERFORM 4640-COUNT-NOTICE
               PERFORM 4650-LIMIT-SURCHG
               PERFORM 4660-ASSESS-EXEMPT
               PERFORM 4670-SET-CHANNEL
               PERFORM 4680-ACCUM-STATUS
               PERFORM 4690-SPLIT-SETTLE
               PERFORM 4700-SET-NETWORK
               PERFORM 4710-PRORATE-ADJUST
               PERFORM 7030-CALL-ATVAL
               REWRITE ATN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ATNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ATN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY NETWORK CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-NETWORK.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-NETWORK-CD
                   ADD ATNM-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-NETWORK-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E855 OPERATOR REQUIRED
      * E282 SURCHG AMT NEGATIVE
      * E117 SURCHG AMT OVER 5000
      * E435 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-OPERATOR = SPACES
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT < 0
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT > 5000.00
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASSETTE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-CASSETTE.
           IF ATNT-CASSETTE-AMT < 50.00
               MOVE 50.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-CASSETTE-AMT > 10000.00
               MOVE 10000.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ATNT-SETTLE-AMT NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG TIER TIER FOR THE CASSETTE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-TIER-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ATNM-CASSETTE-YTD NOT < WS-SURCHG-TIER-LOW(WS-TX)
                   MOVE WS-SURCHG-TIER-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNM-ADJUST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-ADJUST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN EFF DATE IS PAST
      * JJ -> JZ WHEN EFF DATE IS PAST
      * JZ -> KK WHEN EFF DATE IS PAST
      * KK -> BJ WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-ADJUST.
           IF ATNM-ADJUST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-ADJUST-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-YTD * ATNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURCHG AMT.
      *----------------------------------------------------------------*
       3160-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-SURCHG-AMT ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-CASSETTE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE ATNM-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3180-LIMIT-INTERCHG.
           IF ATNM-INTERCHG-AMT < 100.00
               MOVE 100.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-INTERCHG-AMT > 500.00
               MOVE 500.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN EFF DATE IS PAST
      * DP -> FJ WHEN EFF DATE IS PAST
      * FJ -> NA WHEN EFF DATE IS PAST
      * NA -> DF WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE ADJUST YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3210-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = ATNM-ADJUST-YTD *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 7.00 TO ATNT-SURCHG-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 10.00 TO ATNT-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = ATNM-ADJUST-YTD *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNT-SURCHG-AMT > 0 AND ATNT-SURCHG-AMT < 5.00
               MOVE 5.00 TO ATNT-SURCHG-AMT
           END-IF
           IF ATNT-SURCHG-AMT > 35.00
               MOVE 35.00 TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ATNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-DISPENSED.
           IF ATNM-DISPENSED-YTD < 100.00
               MOVE 100.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-DISPENSED-YTD > 1000.00
               MOVE 1000.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG TIER TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-TIER-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ATNM-INTERCHG-YTD NOT < WS-SURCHG-TIER-LOW(WS-TX)
                   MOVE WS-SURCHG-TIER-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISPENSED AMT.
      *----------------------------------------------------------------*
       3250-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISPENSED AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3260-LIMIT-DISPENSED.
           IF ATNT-DISPENSED-AMT < 25.00
               MOVE 25.00 TO ATNT-DISPENSED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-DISPENSED-AMT > 2500.00
               MOVE 2500.00 TO ATNT-DISPENSED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E799 REJECT CD REQUIRED
      * E605 WORK AMT 2 NEGATIVE
      * E475 WORK AMT 2 OVER 99999
      * E550 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-REJECT-CD = SPACES
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN MATURITY DATE IS PAST
      * DP -> FJ WHEN MATURITY DATE IS PAST
      * FJ -> NA WHEN MATURITY DATE IS PAST
      * NA -> DF WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASSETTE YTD.
      *----------------------------------------------------------------*
       3290-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-YTD ROUNDED =
                   ATNM-DISPENSED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-SURCHG.
           IF ATNM-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SURCHG-AMT * ATNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE SETTLE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3310-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-REASON-CD = 'AK'
                   MOVE 5.00 TO ATNM-SURCHG-AMT
               WHEN ATNM-REASON-CD = 'HX'
                   MOVE 10.00 TO ATNM-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-SURCHG-AMT > 0 AND ATNM-SURCHG-AMT < 5.00
               MOVE 5.00 TO ATNM-SURCHG-AMT
           END-IF
           IF ATNM-SURCHG-AMT > 150.00
               MOVE 150.00 TO ATNM-SURCHG-AMT
           END-IF
           ADD ATNM-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-DISPENSED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-DISPENSED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-DISPENSED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-DISPENSED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 5
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3340-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-CHANNEL-CD
                   ADD ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E625 CONTACT REQUIRED
      * E576 SETTLE AMT NEGATIVE
      * E920 SETTLE AMT OVER 25000
      * E267 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 25000.00
                   MOVE 'E920' TO WS-REJECT-CODE
               WHEN ATNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E090 DESC REQUIRED
      * E078 SETTLE AMT NEGATIVE
      * E419 SETTLE AMT OVER 25000
      * E710 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN ATNT-SETTLE-AMT < 0
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN ATNT-SETTLE-AMT > 25000.00
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN ATNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E710' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3370-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTERCHG YTD.
      *----------------------------------------------------------------*
       3380-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-INTERCHG-YTD ROUNDED =
                   ATNM-ADJUST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-INTERCHG-YTD
           END-IF
           ADD ATNM-INTERCHG-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST YTD FEE ON THE SETTLE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3400-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 20.00 TO ATNM-ADJUST-YTD
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 5.00 TO ATNM-ADJUST-YTD
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-ADJUST-YTD > 0 AND ATNM-ADJUST-YTD < 2.00
               MOVE 2.00 TO ATNM-ADJUST-YTD
           END-IF
           IF ATNM-ADJUST-YTD > 35.00
               MOVE 35.00 TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REGION-CD
                   ADD ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-AMT ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE SURCHG YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3450-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED = ATNM-SURCHG-YTD *
                       ATNM-RATE / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 5.00 TO ATNM-CASSETTE-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 20.00 TO ATNM-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED = ATNM-SURCHG-YTD *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-CASSETTE-AMT > 0 AND ATNM-CASSETTE-AMT < 1.00
               MOVE 1.00 TO ATNM-CASSETTE-AMT
           END-IF
           IF ATNM-CASSETTE-AMT > 250.00
               MOVE 250.00 TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN MATURITY DATE IS PAST
      * DP -> FJ WHEN MATURITY DATE IS PAST
      * FJ -> NA WHEN MATURITY DATE IS PAST
      * NA -> DF WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-AMT ROUNDED =
                   ATNM-INTERCHG-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E240 CONTACT REQUIRED
      * E357 SETTLE AMT NEGATIVE
      * E245 SETTLE AMT OVER 50000
      * E904 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 50000.00
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISPENSED YTD.
      *----------------------------------------------------------------*
       3490-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-DISPENSED-YTD ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-DISPENSED-YTD
           END-IF
           ADD ATNM-DISPENSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT SURCHG YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNM-SURCHG-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SURCHG-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-REVIEW-DATE > 0 AND ATNM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADJUST YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-YTD - (WS-WORK-AMT * 3)
           COMPUTE ATNM-DISPENSED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-SETTLE.
           IF ATNT-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SETTLE-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-ADJUST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ATNM-DISPENSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNT-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E758 CUST ID REQUIRED
      * E936 SETTLE AMT NEGATIVE
      * E122 SETTLE AMT OVER 10000
      * E147 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CUST-ID = SPACES
                   MOVE 'E758' TO WS-REJECT-CODE
               WHEN ATNT-SETTLE-AMT < 0
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN ATNT-SETTLE-AMT > 10000.00
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN ATNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-SETTLE.
           IF ATNT-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SETTLE-AMT * ATNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SURCHG-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 3
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISPENSED YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3610-LIMIT-DISPENSED.
           IF ATNM-DISPENSED-YTD < 25.00
               MOVE 25.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-DISPENSED-YTD > 2500.00
               MOVE 2500.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN OPEN DATE IS PAST
      * HX -> YC WHEN OPEN DATE IS PAST
      * YC -> ZB WHEN OPEN DATE IS PAST
      * ZB -> DU WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-YTD - (WS-WORK-AMT *
               4)
           COMPUTE ATNM-CASSETTE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURCHG YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SURCHG-YTD ROUNDED =
                   ATNM-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SURCHG-YTD
           END-IF
           ADD ATNM-SURCHG-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E021 REJECT CD REQUIRED
      * E210 SURCHG AMT NEGATIVE
      * E316 SURCHG AMT OVER 1000000
      * E466 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-REJECT-CD = SPACES
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT < 0
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT > 1000000.00
                   MOVE 'E316' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E466' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE YTD FEE ON THE INTERCHG AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3660-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-YTD ROUNDED = ATNM-INTERCHG-AMT
                       * ATNM-DISCOUNT / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 5.00 TO ATNM-CASSETTE-YTD
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 5.00 TO ATNM-CASSETTE-YTD
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-YTD ROUNDED = ATNM-INTERCHG-AMT
                       * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-CASSETTE-YTD > 0 AND ATNM-CASSETTE-YTD < 2.00
               MOVE 2.00 TO ATNM-CASSETTE-YTD
           END-IF
           IF ATNM-CASSETTE-YTD > 250.00
               MOVE 250.00 TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3680-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-INTERCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASSETTE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3690-FLAG-CASSETTE.
           IF ATNM-CASSETTE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN NOTICE DATE IS PAST
      * G -> H WHEN NOTICE DATE IS PAST
      * H -> P WHEN NOTICE DATE IS PAST
      * P -> V WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-AMT - (WS-WORK-AMT * 12)
           COMPUTE ATNM-ADJUST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E774 NAME REQUIRED
      * E954 WORK AMT 2 NEGATIVE
      * E201 WORK AMT 2 OVER 1000000
      * E815 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E774' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E902 ADDR REQUIRED
      * E782 CASSETTE AMT NEGATIVE
      * E249 CASSETTE AMT OVER 10000
      * E971 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN ATNT-CASSETTE-AMT < 0
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN ATNT-CASSETTE-AMT > 10000.00
                   MOVE 'E249' TO WS-REJECT-CODE
               WHEN ATNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E971' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 0.00
               MOVE 0.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 1000.00
               MOVE 1000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3750-FLAG-CASSETTE.
           IF ATNM-CASSETTE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ATNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-CASSETTE.
           IF ATNT-CASSETTE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-CASSETTE-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNT-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-SETTLE.
           IF ATNM-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SETTLE-AMT * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SURCHG-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ATNT-SETTLE-AMT NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG TIER TIER FOR THE ADJUST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-TIER-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ATNM-ADJUST-AMT NOT < WS-SURCHG-TIER-LOW(WS-TX)
                   MOVE WS-SURCHG-TIER-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-ADJUST.
           IF ATNM-ADJUST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-ADJUST-YTD * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E353 ADDR REQUIRED
      * E389 SURCHG AMT NEGATIVE
      * E086 SURCHG AMT OVER 99999
      * E224 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT < 0
                   MOVE 'E389' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT > 99999.00
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN ATNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE SETTLE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3880-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNT-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNT-DISPENSED-AMT ROUNDED = ATNM-SETTLE-AMT
                       * ATNM-DISCOUNT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO ATNT-DISPENSED-AMT
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO ATNT-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNT-DISPENSED-AMT ROUNDED = ATNM-SETTLE-AMT
                       * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNT-DISPENSED-AMT > 0 AND ATNT-DISPENSED-AMT < 5.00
               MOVE 5.00 TO ATNT-DISPENSED-AMT
           END-IF
           IF ATNT-DISPENSED-AMT > 25.00
               MOVE 25.00 TO ATNT-DISPENSED-AMT
           END-IF
           ADD ATNT-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ADJUST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-YTD - (WS-WORK-AMT * 4)
           COMPUTE ATNT-DISPENSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG FEE ON THE SETTLE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3910-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED = ATNM-SETTLE-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-REASON-CD = 'AK'
                   MOVE 20.00 TO ATNM-INTERCHG-AMT
               WHEN ATNM-REASON-CD = 'HX'
                   MOVE 5.00 TO ATNM-INTERCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED = ATNM-SETTLE-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-INTERCHG-AMT > 0 AND ATNM-INTERCHG-AMT < 10.00
               MOVE 10.00 TO ATNM-INTERCHG-AMT
           END-IF
           IF ATNM-INTERCHG-AMT > 250.00
               MOVE 250.00 TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-TRAN-CD
                   ADD ATNM-INTERCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-INTERCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-CASSETTE.
           IF ATNT-CASSETTE-AMT < 50.00
               MOVE 50.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-CASSETTE-AMT > 1000.00
               MOVE 1000.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E625 ADDR REQUIRED
      * E925 SETTLE AMT NEGATIVE
      * E054 SETTLE AMT OVER 25000
      * E938 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 25000.00
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN ATNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'V'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-ADJUST.
           IF ATNM-ADJUST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-ADJUST-YTD * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-ADJUST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ATNM-ADJUST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE INTERCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ATNM-INTERCHG-AMT NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG TIER TIER FOR THE ADJUST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-TIER-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ATNM-ADJUST-AMT NOT < WS-SURCHG-TIER-LOW(WS-TX)
                   MOVE WS-SURCHG-TIER-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4010-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-YTD ROUNDED = WS-WORK-AMT-2 *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 3.00 TO ATNM-INTERCHG-YTD
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 12.00 TO ATNM-INTERCHG-YTD
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-YTD ROUNDED = WS-WORK-AMT-2 *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-INTERCHG-YTD > 0 AND ATNM-INTERCHG-YTD < 10.00
               MOVE 10.00 TO ATNM-INTERCHG-YTD
           END-IF
           IF ATNM-INTERCHG-YTD > 25.00
               MOVE 25.00 TO ATNM-INTERCHG-YTD
           END-IF
           ADD ATNM-INTERCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST YTD BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNM-ADJUST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-ADJUST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-SURCHG.
           IF ATNT-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SURCHG-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-CASSETTE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E264 CONTACT REQUIRED
      * E260 SURCHG YTD NEGATIVE
      * E853 SURCHG YTD OVER 25000
      * E531 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD < 0
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD > 25000.00
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN ATNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN CLOSE DATE IS PAST
      * HX -> PR WHEN CLOSE DATE IS PAST
      * PR -> XD WHEN CLOSE DATE IS PAST
      * XD -> ZJ WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SURCHG AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNM-SURCHG-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SURCHG-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNM-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4070-FLAG-CASSETTE.
           IF ATNM-CASSETTE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 5
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE CASSETTE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ATNM-CASSETTE-YTD NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SETTLE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4110-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-SURCHG.
           IF ATNM-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SURCHG-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E131 REJECT CD REQUIRED
      * E616 SURCHG AMT NEGATIVE
      * E073 SURCHG AMT OVER 25000
      * E279 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-REJECT-CD = SPACES
                   MOVE 'E131' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT < 0
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT > 25000.00
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN ATNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E279' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-SURCHG.
           IF ATNM-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SURCHG-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-DISPENSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E432 OPERATOR REQUIRED
      * E119 SETTLE AMT NEGATIVE
      * E564 SETTLE AMT OVER 25000
      * E807 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-OPERATOR = SPACES
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 25000.00
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E389 REF NO REQUIRED
      * E578 SETTLE AMT NEGATIVE
      * E376 SETTLE AMT OVER 1000000
      * E660 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-REF-NO = SPACES
                   MOVE 'E389' TO WS-REJECT-CODE
               WHEN ATNT-SETTLE-AMT < 0
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN ATNT-SETTLE-AMT > 1000000.00
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4200-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 50.00
               MOVE 50.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 1000.00
               MOVE 1000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4210-ACCRUE-ADJUST.
           IF ATNM-ADJUST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-ADJUST-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNT-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BI'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ATNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ATNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE CASSETTE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ATNM-CASSETTE-YTD NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4280-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E731 DESC REQUIRED
      * E668 ADJUST AMT NEGATIVE
      * E500 ADJUST AMT OVER 25000
      * E640 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E731' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT < 0
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT > 25000.00
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN ATNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4310-FLAG-SURCHG.
           IF ATNT-SURCHG-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNT-SURCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4320-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REGION-CD
                   ADD ATNM-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4330-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4340-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-YTD * ATNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 6
               MOVE 'Y' TO ATNM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4360-LIMIT-SURCHG.
           IF ATNT-SURCHG-AMT < 10.00
               MOVE 10.00 TO ATNT-SURCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-SURCHG-AMT > 10000.00
               MOVE 10000.00 TO ATNT-SURCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN EXPIRY DATE IS PAST
      * HX -> PR WHEN EXPIRY DATE IS PAST
      * PR -> XD WHEN EXPIRY DATE IS PAST
      * XD -> ZJ WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4370-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4380-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E987 NAME REQUIRED
      * E641 DISPENSED AMT NEGATIVE
      * E774 DISPENSED AMT OVER 99999
      * E090 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4390-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT < 0
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT > 99999.00
                   MOVE 'E774' TO WS-REJECT-CODE
               WHEN ATNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4400-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE ATNM-ADJUST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4420-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE SETTLE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4430-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNT-CASSETTE-AMT ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 10.00 TO ATNT-CASSETTE-AMT
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 7.00 TO ATNT-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATNT-CASSETTE-AMT ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNT-CASSETTE-AMT > 0 AND ATNT-CASSETTE-AMT < 2.00
               MOVE 2.00 TO ATNT-CASSETTE-AMT
           END-IF
           IF ATNT-CASSETTE-AMT > 25.00
               MOVE 25.00 TO ATNT-CASSETTE-AMT
           END-IF
           ADD ATNT-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-SURCHG.
           IF ATNT-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SURCHG-AMT * ATNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATNM-DISPENSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISPENSED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4450-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REGION-CD
                   ADD ATNT-DISPENSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-DISPENSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4460-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 6
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4470-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4480-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNM-ADJUST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-ADJUST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4490-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4500-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 25.00
               MOVE 25.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 50000.00
               MOVE 50000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4510-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-TRAN-CD
                   ADD ATNM-ADJUST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-ADJUST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4520-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4530-LIMIT-DISPENSED.
           IF ATNM-DISPENSED-YTD < 0.00
               MOVE 0.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-DISPENSED-YTD > 5000.00
               MOVE 5000.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4540-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4550-FLAG-INTERCHG.
           IF ATNM-INTERCHG-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-INTERCHG-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATNM-INTERCHG-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-INTERCHG-YTD - (WS-WORK-AMT * 4)
           COMPUTE ATNT-DISPENSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4570-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4580-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4590-ACCRUE-SURCHG.
           IF ATNM-SURCHG-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SURCHG-YTD * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4600-FLAG-DISPENSED.
           IF ATNM-DISPENSED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURCHG AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4610-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNT-SURCHG-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ATNT-SURCHG-AMT - (WS-WORK-AMT * 12)
           COMPUTE ATNM-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE INTERCHG YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4620-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = ATNM-INTERCHG-YTD *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-REASON-CD = 'AK'
                   MOVE 10.00 TO ATNT-SURCHG-AMT
               WHEN ATNM-REASON-CD = 'HX'
                   MOVE 15.00 TO ATNT-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = ATNM-INTERCHG-YTD *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNT-SURCHG-AMT > 0 AND ATNT-SURCHG-AMT < 2.00
               MOVE 2.00 TO ATNT-SURCHG-AMT
           END-IF
           IF ATNT-SURCHG-AMT > 100.00
               MOVE 100.00 TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG YTD FEE ON THE SETTLE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4630-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-YTD ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-DISCOUNT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO ATNM-INTERCHG-YTD
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO ATNM-INTERCHG-YTD
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-YTD ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-INTERCHG-YTD > 0 AND ATNM-INTERCHG-YTD < 1.00
               MOVE 1.00 TO ATNM-INTERCHG-YTD
           END-IF
           IF ATNM-INTERCHG-YTD > 150.00
               MOVE 150.00 TO ATNM-INTERCHG-YTD
           END-IF
           ADD ATNM-INTERCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4640-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4650-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 25.00
               MOVE 25.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 5000.00
               MOVE 5000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE DISPENSED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4660-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-RATE / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 3.00 TO ATNM-DISPENSED-AMT
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 3.00 TO ATNM-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-DISPENSED-AMT > 0 AND ATNM-DISPENSED-AMT < 1.00
               MOVE 1.00 TO ATNM-DISPENSED-AMT
           END-IF
           IF ATNM-DISPENSED-AMT > 75.00
               MOVE 75.00 TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4670-SET-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4680-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNM-INTERCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-INTERCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4690-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNM-SETTLE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-SETTLE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN NOTICE DATE IS PAST
      * C -> H WHEN NOTICE DATE IS PAST
      * H -> R WHEN NOTICE DATE IS PAST
      * R -> S WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4700-SET-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       4710-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-YTD ROUNDED =
                   ATNM-DISPENSED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE ATN FIELDS TO ATVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-ATVAL.
           INITIALIZE ATVAL-PARMS
           MOVE ATNM-ADJUST-AMT
             TO ATVAL-ADJUST-AMT
           MOVE ATNM-INTERCHG-YTD
             TO ATVAL-INTERCHG-YTD
           MOVE ATNM-CASSETTE-AMT
             TO ATVAL-CASSETTE-AMT
           MOVE ATNM-SETTLE-YTD
             TO ATVAL-SETTLE-YTD
           MOVE ATNM-SURCHG-AMT
             TO ATVAL-SURCHG-AMT
           MOVE ATNM-SURCHG-YTD
             TO ATVAL-SURCHG-YTD
           MOVE ATNM-NOTICE-DATE
             TO ATVAL-NOTICE-DATE
           MOVE ATNM-NEXT-REVIEW-DATE
             TO ATVAL-NEXT-REVIEW-DATE
           MOVE ATNM-MATURITY-DATE
             TO ATVAL-MATURITY-DATE
           MOVE ATNM-OPEN-DATE
             TO ATVAL-OPEN-DATE
           MOVE ATNM-TERMINAL-CD
             TO ATVAL-TERMINAL-CD
           MOVE ATNM-BRANCH-CD
             TO ATVAL-BRANCH-CD
           MOVE ATNM-STATUS-CD
             TO ATVAL-STATUS-CD
           MOVE ATNM-REASON-CD
             TO ATVAL-REASON-CD
           MOVE ATNM-SUSPEND-FLAG
             TO ATVAL-SUSPEND-FLAG
           MOVE ATNM-EXEMPT-FLAG
             TO ATVAL-EXEMPT-FLAG
           MOVE ATNM-REVIEW-FLAG
             TO ATVAL-REVIEW-FLAG
           MOVE ATNM-NOTICE-CNT
             TO ATVAL-NOTICE-CNT
           MOVE ATNM-PAYMENT-CNT
             TO ATVAL-PAYMENT-CNT
           MOVE ATNM-DISCOUNT
             TO ATVAL-DISCOUNT
           MOVE ATNM-RATE
             TO ATVAL-RATE
           MOVE ATNM-CONTACT
             TO ATVAL-CONTACT
           MOVE ATNM-CUST-ID
             TO ATVAL-CUST-ID
           CALL 'ATVAL' USING ATVAL-PARMS
           EVALUATE ATVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ATVAL-SETTLE-YTD
                     TO ATNM-SETTLE-YTD
                   MOVE ATVAL-SURCHG-AMT
                     TO ATNM-SURCHG-AMT
                   MOVE ATVAL-SURCHG-YTD
                     TO ATNM-SURCHG-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-ATVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ATNT-KEY TO RD-KEY
           MOVE ATNT-TRAN-CD TO RD-CODE
           MOVE ATNT-SURCHG-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ATNT-KEY TO RD-KEY
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
           DISPLAY 'ATNPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ATNPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
