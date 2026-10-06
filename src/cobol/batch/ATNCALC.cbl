      *================================================================*
      * PROGRAM  : ATNCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ATM NETWORK SETTLEMENT - PERIODIC CALCULATION. READS*
      *            THE ATN TRANSACTIONS, UPDATES THE ATN MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. PROPRIETARY SHARED ATM *
      *            NETWORK SETTLEMENT AND INTERCHANGE                  *
      * INPUT    : ATNMST (KSDS), ATNTRN, BANKCTL                      *
      * OUTPUT   : ATNOUT, ATNCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ATVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-08 JLM  ORIGINAL                                       *
      * 1997-03-08 DPS  INTERCHG LIMIT REVISED PRB8919                 *
      * 1999-07-14 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-10-17 LMB  ADJUST NOTICE TEXT PER LEGAL PRB5747           *
      * 2004-07-03 BHS  DISPENSED TABLE EXPANDED REQ6227               *
      * 2007-07-18 DPS  CASSETTE LIMIT REVISED PRB1065                 *
      * 2007-05-10 TWH  INTERCHG ROUNDING - HALF UP PRB4856            *
      * 2008-12-12 RJT  CASSETTE ROUNDING - HALF UP REQ1249            *
      * 2008-11-25 EWS  AUDIT FINDING - ADJUST CONTROL TOTALS PRB6982  *
      * 2011-10-10 JLM  CASSETTE EXCEPTIONS TO OPS QUEUE CHG5333       *
      * 2013-10-06 DPS  DISPENSED CALCULATION CORRECTED CHG0559        *
      * 2014-08-28 DPS  SURCHG LIMIT REVISED PRB1644                   *
      * 2014-05-02 PQR  PERFORMANCE - CASSETTE READ AHEAD PRB4710      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATNCALC.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "ATNCARPT"
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
                                           VALUE 8.000000.
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
      * WS-DISPENSE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISPENSE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000822000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000268000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000636500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000213500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000302500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000237500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008350000000472000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000530000'.
       01  WS-DISPENSE-BAND-TBL REDEFINES WS-DISPENSE-BAND-VALUES.
           05  WS-DISPENSE-BAND-ROW OCCURS 8 TIMES.
               10  WS-DISPENSE-BAND-LOW    PIC 9(09)V99.
               10  WS-DISPENSE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISPENSE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISPENSE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000624000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000441000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000135500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000673000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000759500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000026500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000666000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000215000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011150000000511000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000464000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000465000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000318500'.
       01  WS-DISPENSE-TIER-TBL REDEFINES WS-DISPENSE-TIER-VALUES.
           05  WS-DISPENSE-TIER-ROW OCCURS 12 TIMES.
               10  WS-DISPENSE-TIER-LOW    PIC 9(09)V99.
               10  WS-DISPENSE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISPENSE-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISPENSE-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000887500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000438000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000811500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000652000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000678500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000007000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000264000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026100000000527500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026200000000845000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026300000000087000'.
       01  WS-DISPENSE-TIERX-TBL REDEFINES WS-DISPENSE-TIERX-VALUES.
           05  WS-DISPENSE-TIERX-ROW OCCURS 11 TIMES.
               10  WS-DISPENSE-TIERX-LOW   PIC 9(09)V99.
               10  WS-DISPENSE-TIERX-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ATNCALC  ATM NETWORK SETTLEMENT'.
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
               PERFORM 3000-CHECK-MATURITY
               PERFORM 3010-AGE-EXPIRY
               PERFORM 3020-CHECK-LAST-PAY
               PERFORM 3030-CHECK-MATURITY
               PERFORM 3040-COUNT-RETRY
               PERFORM 3050-CHECK-EFFECTIVE
               PERFORM 3060-PRORATE-CASSETTE
               PERFORM 3070-ACCUM-STATUS
               PERFORM 3080-PRORATE-DISPENSED
               PERFORM 3090-ACCUM-BRANCH
               PERFORM 3100-EDIT-MEMO
               PERFORM 3110-DESCRIBE-CHANNEL
               PERFORM 3120-SPLIT-SETTLE
               PERFORM 3130-CHECK-POST
               PERFORM 3140-PRORATE-ADJUST
               PERFORM 3150-SPLIT-INTERCHG
               PERFORM 3160-PRORATE-DISPENSED
               PERFORM 3170-ACCUM-BRANCH
               PERFORM 3180-SELECT-RATE
               PERFORM 3190-FLAG-SURCHG
               PERFORM 3200-CHECK-NEXT-DUE
               PERFORM 3210-SELECT-FEE-PCT
               PERFORM 3220-EDIT-CONTACT
               PERFORM 3230-SELECT-RATE
               PERFORM 3240-PRORATE-SETTLE
               PERFORM 3250-ACCRUE-DISPENSED
               PERFORM 3260-EDIT-NAME
               PERFORM 3270-COUNT-NOTICE
               PERFORM 3280-ACCRUE-SETTLE
               PERFORM 3290-AGE-MATURITY
               PERFORM 3300-AGE-NEXT-REVIEW
               PERFORM 3310-ACCUM-CHANNEL
               PERFORM 3320-LIMIT-ADJUST
               PERFORM 3330-SET-STATUS
               PERFORM 3340-CHECK-EXPIRY
               PERFORM 3350-LIMIT-CASSETTE
               PERFORM 3360-EDIT-ADDR
               PERFORM 3370-COUNT-SEQ
               PERFORM 3380-SELECT-RATE
               PERFORM 3390-AGE-LAST-PAY
               PERFORM 3400-ACCRUE-DISPENSED
               PERFORM 3410-AGE-MATURITY
               PERFORM 3420-ASSESS-SUSPEND
               PERFORM 3430-LIMIT-ITEM
               PERFORM 3440-SELECT-FEE-PCT
               PERFORM 3450-ACCUM-NETWORK
               PERFORM 3460-SPLIT-SURCHG
               PERFORM 3470-SPLIT-ITEM
               PERFORM 3480-FLAG-SETTLE
               PERFORM 3490-SET-STATUS
               PERFORM 3500-ACCRUE-INTERCHG
               PERFORM 3510-ACCUM-REASON
               PERFORM 3520-FLAG-CASSETTE
               PERFORM 3530-CHECK-CLOSE
               PERFORM 3540-ASSESS-EXEMPT
               PERFORM 3550-FLAG-DISPENSED
               PERFORM 3560-AGE-CLOSE
               PERFORM 3570-SPLIT-DISPENSED
               PERFORM 3580-ACCUM-CHANNEL
               PERFORM 3590-COUNT-RETRY
               PERFORM 3600-EDIT-ADDR
               PERFORM 3610-COUNT-SEQ
               PERFORM 3620-LIMIT-INTERCHG
               PERFORM 3630-EDIT-DESC
               PERFORM 3640-COUNT-ITEM
               PERFORM 3650-ASSESS-NOTICE-SENT
               PERFORM 3660-ACCRUE-SURCHG
               PERFORM 3670-PRORATE-SURCHG
               PERFORM 3680-PRORATE-DISPENSED
               PERFORM 3690-PRORATE-CASSETTE
               PERFORM 3700-EDIT-CUST-ID
               PERFORM 3710-EDIT-CONTACT
               PERFORM 3720-EDIT-NAME
               PERFORM 3730-SET-BRANCH
               PERFORM 3740-PRORATE-ADJUST
               PERFORM 3750-PRORATE-SETTLE
               PERFORM 3760-CHECK-MATURITY
               PERFORM 3770-ASSESS-SUSPEND
               PERFORM 3780-ACCUM-NETWORK
               PERFORM 3790-AGE-POST
               PERFORM 3800-CHECK-EXPIRY
               PERFORM 3810-EDIT-DESC
               PERFORM 3820-CHECK-NOTICE
               PERFORM 3830-COUNT-PAYMENT
               PERFORM 3840-COUNT-NOTICE
               PERFORM 3850-CHECK-EFF
               PERFORM 3860-ACCUM-TRAN
               PERFORM 3870-AGE-CLOSE
               PERFORM 3880-EDIT-CONTACT
               PERFORM 3890-PRORATE-SETTLE
               PERFORM 3900-SPLIT-ADJUST
               PERFORM 3910-DESCRIBE-REGION
               PERFORM 3920-LIMIT-ADJUST
               PERFORM 3930-CHECK-EXPIRY
               PERFORM 3940-SPLIT-DISPENSED
               PERFORM 3950-SELECT-DISCOUNT
               PERFORM 3960-SPLIT-INTERCHG
               PERFORM 3970-COUNT-PAYMENT
               PERFORM 3980-SET-NETWORK
               PERFORM 3990-PRORATE-SURCHG
               PERFORM 4000-DESCRIBE-CHANNEL
               PERFORM 4010-COUNT-NOTICE
               PERFORM 4020-SET-REASON
               PERFORM 4030-ASSESS-SUSPEND
               PERFORM 4040-ACCUM-BRANCH
               PERFORM 4050-DESCRIBE-TERMINAL
               PERFORM 4060-ASSESS-STOP
               PERFORM 4070-ACCUM-NETWORK
               PERFORM 4080-AGE-EFFECTIVE
               PERFORM 4090-COUNT-SEQ
               PERFORM 4100-ACCUM-STATUS
               PERFORM 4110-SELECT-DISCOUNT
               PERFORM 4120-PRORATE-DISPENSED
               PERFORM 4130-AGE-MATURITY
               PERFORM 4140-PRORATE-CASSETTE
               PERFORM 4150-ACCRUE-FEE-PCT
               PERFORM 4160-EDIT-ADDR
               PERFORM 4170-COUNT-SEQ
               PERFORM 4180-LIMIT-CASSETTE
               PERFORM 4190-SPLIT-SURCHG
               PERFORM 4200-ASSESS-HOLD
               PERFORM 4210-FLAG-SURCHG
               PERFORM 4220-DESCRIBE-BRANCH
               PERFORM 4230-SELECT-RATE
               PERFORM 4240-ACCRUE-CASSETTE
               PERFORM 4250-AGE-CLOSE
               PERFORM 4260-LIMIT-SURCHG
               PERFORM 4270-LIMIT-SURCHG
               PERFORM 4280-AGE-LAST-PAY
               PERFORM 4290-FLAG-ITEM
               PERFORM 4300-LIMIT-SURCHG
               PERFORM 4310-SELECT-RATE
               PERFORM 4320-ASSESS-HOLD
               PERFORM 4330-LIMIT-CASSETTE
               PERFORM 4340-PRORATE-DISPENSED
               PERFORM 4350-PRORATE-ADJUST
               PERFORM 4360-LIMIT-CASSETTE
               PERFORM 4370-PRORATE-SURCHG
               PERFORM 4380-SPLIT-ADJUST
               PERFORM 4390-SPLIT-INTERCHG
               PERFORM 4400-SET-TERMINAL
               PERFORM 4410-FLAG-DISPENSED
               PERFORM 4420-ACCUM-STATUS
               PERFORM 4430-SPLIT-ADJUST
               PERFORM 4440-SPLIT-ADJUST
               PERFORM 4450-ASSESS-EXEMPT
               PERFORM 4460-EDIT-OPERATOR
               PERFORM 4470-ACCRUE-DISPENSED
               PERFORM 4480-COUNT-ITEM
               PERFORM 4490-EDIT-CONTACT
               PERFORM 4500-FLAG-CASSETTE
               PERFORM 4510-EDIT-NAME
               PERFORM 4520-ACCUM-STATUS
               PERFORM 4530-DESCRIBE-BRANCH
               PERFORM 4540-FLAG-SURCHG
               PERFORM 4550-DESCRIBE-REASON
               PERFORM 4560-ASSESS-SUSPEND
               PERFORM 4570-SELECT-DISCOUNT
               PERFORM 4580-ACCUM-BRANCH
               PERFORM 4590-ACCRUE-DISPENSED
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
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-MATURITY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-PAY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-MATURITY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ATNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASSETTE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3060-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-CASSETTE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3080-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-DISPENSED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E462 MEMO REQUIRED
      * E376 WORK AMT 2 NEGATIVE
      * E278 WORK AMT 2 OVER 250000
      * E015 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-MEMO = SPACES
                   MOVE 'E462' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E278' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNT-SETTLE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ATNT-SETTLE-AMT - (WS-WORK-AMT * 2)
           COMPUTE ATNM-ADJUST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-POST.
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
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADJUST AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-AMT ROUNDED =
                   ATNT-SURCHG-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATNM-INTERCHG-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-INTERCHG-YTD - (WS-WORK-AMT * 4)
           COMPUTE ATNT-SURCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASSETTE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISPENSED YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-DISPENSED-YTD ROUNDED =
                   ATNT-CASSETTE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-DISPENSED-YTD
           END-IF
           ADD ATNM-DISPENSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE BAND TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNM-DISPENSED-AMT NOT < WS-DISPENSE-BAND-LOW(WS-TX)
                   MOVE WS-DISPENSE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3190-FLAG-SURCHG.
           IF ATNM-SURCHG-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-SURCHG-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE TIERX TIER FOR THE CASSETTE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-TIERX-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ATNT-CASSETTE-AMT NOT < WS-DISPENSE-TIERX-LOW(WS-TX)
                   MOVE WS-DISPENSE-TIERX-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E568 CONTACT REQUIRED
      * E489 CASSETTE YTD NEGATIVE
      * E806 CASSETTE YTD OVER 1000000
      * E894 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E568' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD < 0
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD > 1000000.00
                   MOVE 'E806' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE TIER TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-TIER-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATNM-INTERCHG-YTD NOT < WS-DISPENSE-TIER-LOW(WS-TX)
                   MOVE WS-DISPENSE-TIER-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-DISPENSED.
           IF ATNT-DISPENSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-DISPENSED-AMT * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E957 NAME REQUIRED
      * E003 SETTLE YTD NEGATIVE
      * E370 SETTLE YTD OVER 1000000
      * E715 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-YTD < 0
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-YTD > 1000000.00
                   MOVE 'E370' TO WS-REJECT-CODE
               WHEN ATNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3270-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-SETTLE.
           IF ATNM-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SETTLE-AMT * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-MATURITY.
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
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-NEXT-REVIEW.
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
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISPENSED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-CHANNEL-CD
                   ADD ATNM-DISPENSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-DISPENSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-ADJUST.
           IF ATNM-ADJUST-AMT < 50.00
               MOVE 50.00 TO ATNM-ADJUST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-ADJUST-AMT > 10000.00
               MOVE 10000.00 TO ATNM-ADJUST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN MATURITY DATE IS PAST
      * DP -> FJ WHEN MATURITY DATE IS PAST
      * FJ -> NA WHEN MATURITY DATE IS PAST
      * NA -> DF WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-STATUS.
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
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-EXPIRY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3350-LIMIT-CASSETTE.
           IF ATNM-CASSETTE-AMT < 25.00
               MOVE 25.00 TO ATNM-CASSETTE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-CASSETTE-AMT > 500.00
               MOVE 500.00 TO ATNM-CASSETTE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E476 ADDR REQUIRED
      * E238 SETTLE AMT NEGATIVE
      * E065 SETTLE AMT OVER 10000
      * E558 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 10000.00
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN ATNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E558' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 3
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE TIER TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-TIER-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATNM-SETTLE-YTD NOT < WS-DISPENSE-TIER-LOW(WS-TX)
                   MOVE WS-DISPENSE-TIER-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-LAST-PAY.
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
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3400-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-MATURITY.
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
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST YTD FEE ON THE SETTLE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3420-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNM-SETTLE-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-REASON-CD = 'AK'
                   MOVE 15.00 TO ATNM-ADJUST-YTD
               WHEN ATNM-REASON-CD = 'HX'
                   MOVE 15.00 TO ATNM-ADJUST-YTD
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNM-SETTLE-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-ADJUST-YTD > 0 AND ATNM-ADJUST-YTD < 5.00
               MOVE 5.00 TO ATNM-ADJUST-YTD
           END-IF
           IF ATNM-ADJUST-YTD > 50.00
               MOVE 50.00 TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE TIERX TIER FOR THE ADJUST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-TIERX-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ATNM-ADJUST-YTD NOT < WS-DISPENSE-TIERX-LOW(WS-TX)
                   MOVE WS-DISPENSE-TIERX-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY NETWORK CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-NETWORK.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-NETWORK-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-NETWORK-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURCHG AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNM-SURCHG-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SURCHG-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNM-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3480-FLAG-SETTLE.
           IF ATNT-SETTLE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNT-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN POST DATE IS PAST
      * DP -> FJ WHEN POST DATE IS PAST
      * FJ -> NA WHEN POST DATE IS PAST
      * NA -> DF WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-INTERCHG.
           IF ATNM-INTERCHG-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-INTERCHG-YTD * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-DISPENSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-CASSETTE.
           IF ATNM-CASSETTE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-CLOSE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ATNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE CASSETTE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3540-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = ATNM-CASSETTE-AMT *
                       ATNM-RATE / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 3.00 TO ATNT-SURCHG-AMT
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 7.00 TO ATNT-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = ATNM-CASSETTE-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNT-SURCHG-AMT > 0 AND ATNT-SURCHG-AMT < 2.00
               MOVE 2.00 TO ATNT-SURCHG-AMT
           END-IF
           IF ATNT-SURCHG-AMT > 250.00
               MOVE 250.00 TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISPENSED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3550-FLAG-DISPENSED.
           IF ATNM-DISPENSED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE ATNT-DISPENSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-CHANNEL-CD
                   ADD ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E723 ADDR REQUIRED
      * E741 CASSETTE YTD NEGATIVE
      * E375 CASSETTE YTD OVER 1000000
      * E354 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E723' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD < 0
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD > 1000000.00
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 10
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-INTERCHG.
           IF ATNM-INTERCHG-YTD < 10.00
               MOVE 10.00 TO ATNM-INTERCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-INTERCHG-YTD > 5000.00
               MOVE 5000.00 TO ATNM-INTERCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E276 DESC REQUIRED
      * E120 CASSETTE AMT NEGATIVE
      * E923 CASSETTE AMT OVER 50000
      * E087 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN ATNT-CASSETTE-AMT < 0
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN ATNT-CASSETTE-AMT > 50000.00
                   MOVE 'E923' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 3
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG YTD FEE ON THE ADJUST AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3650-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-YTD ROUNDED = ATNM-ADJUST-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-BRANCH-CD = 'DU'
                   MOVE 10.00 TO ATNM-SURCHG-YTD
               WHEN ATNM-BRANCH-CD = 'HX'
                   MOVE 15.00 TO ATNM-SURCHG-YTD
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-YTD ROUNDED = ATNM-ADJUST-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-SURCHG-YTD > 0 AND ATNM-SURCHG-YTD < 2.00
               MOVE 2.00 TO ATNM-SURCHG-YTD
           END-IF
           IF ATNM-SURCHG-YTD > 35.00
               MOVE 35.00 TO ATNM-SURCHG-YTD
           END-IF
           ADD ATNM-SURCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-SURCHG.
           IF ATNT-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SURCHG-AMT * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURCHG AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-SURCHG-AMT ROUNDED =
                   ATNT-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISPENSED AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-DISPENSED-AMT ROUNDED =
                   ATNT-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNT-DISPENSED-AMT
           END-IF
           ADD ATNT-DISPENSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CASSETTE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3690-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-CASSETTE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E127 CUST ID REQUIRED
      * E187 INTERCHG AMT NEGATIVE
      * E410 INTERCHG AMT OVER 50000
      * E581 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CUST-ID = SPACES
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN ATNM-INTERCHG-AMT < 0
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN ATNM-INTERCHG-AMT > 50000.00
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN ATNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E224 CONTACT REQUIRED
      * E918 SURCHG AMT NEGATIVE
      * E773 SURCHG AMT OVER 50000
      * E817 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT < 0
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT > 50000.00
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E817' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E765 NAME REQUIRED
      * E469 DISPENSED AMT NEGATIVE
      * E124 DISPENSED AMT OVER 25000
      * E831 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT < 0
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT > 25000.00
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN ATNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN NEXT REVIEW DATE IS PAST
      * HX -> YC WHEN NEXT REVIEW DATE IS PAST
      * YC -> ZB WHEN NEXT REVIEW DATE IS PAST
      * ZB -> DU WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3730-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3740-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-ADJUST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3750-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-SETTLE-AMT ROUNDED =
                   ATNM-ADJUST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-MATURITY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG YTD FEE ON THE CASSETTE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3770-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-YTD ROUNDED = ATNM-CASSETTE-YTD
                       * ATNM-RATE / 200
               WHEN ATNM-BRANCH-CD = 'DU'
                   MOVE 3.00 TO ATNM-INTERCHG-YTD
               WHEN ATNM-BRANCH-CD = 'HX'
                   MOVE 12.00 TO ATNM-INTERCHG-YTD
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-YTD ROUNDED = ATNM-CASSETTE-YTD
                       * ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-INTERCHG-YTD > 0 AND ATNM-INTERCHG-YTD < 10.00
               MOVE 10.00 TO ATNM-INTERCHG-YTD
           END-IF
           IF ATNM-INTERCHG-YTD > 150.00
               MOVE 150.00 TO ATNM-INTERCHG-YTD
           END-IF
           ADD ATNM-INTERCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY NETWORK CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-NETWORK.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-NETWORK-CD
                   ADD ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-NETWORK-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-POST.
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
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-EXPIRY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E116 DESC REQUIRED
      * E964 SETTLE AMT NEGATIVE
      * E018 SETTLE AMT OVER 25000
      * E769 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E116' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 25000.00
                   MOVE 'E018' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-NOTICE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-EFF.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-CLOSE.
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
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E661 CONTACT REQUIRED
      * E802 WORK AMT NEGATIVE
      * E109 WORK AMT OVER 250000
      * E340 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASSETTE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-AMT ROUNDED =
                   ATNT-CASSETTE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-AMT - (WS-WORK-AMT * 2)
           COMPUTE ATNM-CASSETTE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADJUST YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-ADJUST.
           IF ATNM-ADJUST-YTD < 100.00
               MOVE 100.00 TO ATNM-ADJUST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-ADJUST-YTD > 10000.00
               MOVE 10000.00 TO ATNM-ADJUST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-EXPIRY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-YTD - (WS-WORK-AMT *
               2)
           COMPUTE ATNT-DISPENSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE TIER TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-TIER-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATNM-DISPENSED-AMT NOT < WS-DISPENSE-TIER-LOW(WS-TX)
                   MOVE WS-DISPENSE-TIER-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATNM-INTERCHG-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-INTERCHG-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNT-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN MATURITY DATE IS PAST
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> R WHEN MATURITY DATE IS PAST
      * R -> S WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3990-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNT-SURCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO ATNM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN EXPIRY DATE IS PAST
      * HX -> PR WHEN EXPIRY DATE IS PAST
      * PR -> XD WHEN EXPIRY DATE IS PAST
      * XD -> ZJ WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-REASON.
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
      * ASSESS THE ADJUST FEE ON THE INTERCHG AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4030-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-REASON-CD = 'AK'
                   MOVE 7.00 TO ATNM-ADJUST-AMT
               WHEN ATNM-REASON-CD = 'HX'
                   MOVE 10.00 TO ATNM-ADJUST-AMT
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-ADJUST-AMT > 0 AND ATNM-ADJUST-AMT < 2.00
               MOVE 2.00 TO ATNM-ADJUST-AMT
           END-IF
           IF ATNM-ADJUST-AMT > 50.00
               MOVE 50.00 TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-BRANCH.
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
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE SETTLE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4060-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNT-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNT-DISPENSED-AMT ROUNDED = ATNM-SETTLE-YTD
                       * ATNM-RATE / 200
               WHEN ATNM-BRANCH-CD = 'DU'
                   MOVE 3.00 TO ATNT-DISPENSED-AMT
               WHEN ATNM-BRANCH-CD = 'HX'
                   MOVE 5.00 TO ATNT-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNT-DISPENSED-AMT ROUNDED = ATNM-SETTLE-YTD
                       * ATNM-RATE / 100
           END-EVALUATE
           IF ATNT-DISPENSED-AMT > 0 AND ATNT-DISPENSED-AMT < 5.00
               MOVE 5.00 TO ATNT-DISPENSED-AMT
           END-IF
           IF ATNT-DISPENSED-AMT > 250.00
               MOVE 250.00 TO ATNT-DISPENSED-AMT
           END-IF
           ADD ATNT-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG YTD BY NETWORK CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-NETWORK.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-NETWORK-CD
                   ADD ATNM-INTERCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-NETWORK-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-INTERCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-EFFECTIVE.
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
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 6
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE TIER TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-TIER-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATNT-SETTLE-AMT NOT < WS-DISPENSE-TIER-LOW(WS-TX)
                   MOVE WS-DISPENSE-TIER-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISPENSED AMT.
      *----------------------------------------------------------------*
       4120-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-DISPENSED-AMT ROUNDED =
                   ATNT-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNT-DISPENSED-AMT
           END-IF
           ADD ATNT-DISPENSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-MATURITY.
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
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASSETTE AMT.
      *----------------------------------------------------------------*
       4140-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-AMT ROUNDED =
                   ATNM-INTERCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SURCHG-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E832 ADDR REQUIRED
      * E247 CASSETTE YTD NEGATIVE
      * E327 CASSETTE YTD OVER 1000000
      * E361 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E832' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD < 0
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD > 1000000.00
                   MOVE 'E327' TO WS-REJECT-CODE
               WHEN ATNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 6
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4180-LIMIT-CASSETTE.
           IF ATNT-CASSETTE-AMT < 25.00
               MOVE 25.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-CASSETTE-AMT > 50000.00
               MOVE 50000.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE ATNT-SURCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE SURCHG AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4200-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 10.00 TO ATNM-SETTLE-AMT
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 7.00 TO ATNM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-SETTLE-AMT > 0 AND ATNM-SETTLE-AMT < 2.00
               MOVE 2.00 TO ATNM-SETTLE-AMT
           END-IF
           IF ATNM-SETTLE-AMT > 25.00
               MOVE 25.00 TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SURCHG AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4210-FLAG-SURCHG.
           IF ATNT-SURCHG-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNT-SURCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE BAND TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNM-INTERCHG-YTD NOT < WS-DISPENSE-BAND-LOW(WS-TX)
                   MOVE WS-DISPENSE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4240-ACCRUE-CASSETTE.
           IF ATNT-CASSETTE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-CASSETTE-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-DISPENSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-CLOSE.
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
                   ADD ATNM-ADJUST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-ADJUST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-ADJUST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-ADJUST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-ADJUST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4260-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 100.00
               MOVE 100.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 50000.00
               MOVE 50000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4270-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 10.00
               MOVE 10.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 500.00
               MOVE 500.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4280-AGE-LAST-PAY.
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
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4290-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4300-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 10.00
               MOVE 10.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 10000.00
               MOVE 10000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE BAND TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNT-DISPENSED-AMT NOT < WS-DISPENSE-BAND-LOW(WS-TX)
                   MOVE WS-DISPENSE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE INTERCHG AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4320-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 7.00 TO ATNM-SURCHG-AMT
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 7.00 TO ATNM-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-SURCHG-AMT > 0 AND ATNM-SURCHG-AMT < 1.00
               MOVE 1.00 TO ATNM-SURCHG-AMT
           END-IF
           IF ATNM-SURCHG-AMT > 35.00
               MOVE 35.00 TO ATNM-SURCHG-AMT
           END-IF
           ADD ATNM-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASSETTE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4330-LIMIT-CASSETTE.
           IF ATNM-CASSETTE-AMT < 0.00
               MOVE 0.00 TO ATNM-CASSETTE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-CASSETTE-AMT > 5000.00
               MOVE 5000.00 TO ATNM-CASSETTE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISPENSED AMT.
      *----------------------------------------------------------------*
       4340-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                   ATNM-SURCHG-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       4350-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-YTD ROUNDED =
                   ATNM-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASSETTE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4360-LIMIT-CASSETTE.
           IF ATNT-CASSETTE-AMT < 100.00
               MOVE 100.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-CASSETTE-AMT > 2500.00
               MOVE 2500.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURCHG AMT.
      *----------------------------------------------------------------*
       4370-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-SURCHG-AMT ROUNDED =
                   ATNM-SETTLE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-AMT - (WS-WORK-AMT * 4)
           COMPUTE ATNM-SURCHG-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4390-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE ATNM-INTERCHG-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN NOTICE DATE IS PAST
      * JJ -> JZ WHEN NOTICE DATE IS PAST
      * JZ -> KK WHEN NOTICE DATE IS PAST
      * KK -> BJ WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISPENSED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4410-FLAG-DISPENSED.
           IF ATNM-DISPENSED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4420-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNM-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-YTD - (WS-WORK-AMT * 4)
           COMPUTE ATNT-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4440-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNT-SURCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE DISPENSED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4450-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-DISPENSED-YTD
                       * ATNM-DISCOUNT / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 5.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 15.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-DISPENSED-YTD
                       * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 2.00
               MOVE 2.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 100.00
               MOVE 100.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E833 OPERATOR REQUIRED
      * E815 SURCHG YTD NEGATIVE
      * E046 SURCHG YTD OVER 25000
      * E215 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4460-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-OPERATOR = SPACES
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD < 0
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD > 25000.00
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN ATNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4470-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-YTD * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 5
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E500 CONTACT REQUIRED
      * E318 SURCHG AMT NEGATIVE
      * E649 SURCHG AMT OVER 1000000
      * E929 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4490-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT < 0
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT > 1000000.00
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN ATNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E929' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4500-FLAG-CASSETTE.
           IF ATNM-CASSETTE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E796 NAME REQUIRED
      * E998 SETTLE AMT NEGATIVE
      * E545 SETTLE AMT OVER 25000
      * E083 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4510-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 25000.00
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4520-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4530-DESCRIBE-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4540-FLAG-SURCHG.
           IF ATNM-SURCHG-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-SURCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4550-DESCRIBE-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4560-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED = WS-WORK-AMT *
                       ATNM-RATE / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 20.00 TO ATNM-DISPENSED-AMT
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 3.00 TO ATNM-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED = WS-WORK-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-DISPENSED-AMT > 0 AND ATNM-DISPENSED-AMT < 1.00
               MOVE 1.00 TO ATNM-DISPENSED-AMT
           END-IF
           IF ATNM-DISPENSED-AMT > 25.00
               MOVE 25.00 TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DISPENSE TIER TIER FOR THE CASSETTE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4570-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISPENSE-TIER-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATNM-CASSETTE-AMT NOT < WS-DISPENSE-TIER-LOW(WS-TX)
                   MOVE WS-DISPENSE-TIER-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4580-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNM-INTERCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-INTERCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4590-ACCRUE-DISPENSED.
           IF ATNT-DISPENSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-DISPENSED-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

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
                   MOVE ATVAL-ADJUST-AMT
                     TO ATNM-ADJUST-AMT
                   MOVE ATVAL-INTERCHG-YTD
                     TO ATNM-INTERCHG-YTD
                   MOVE ATVAL-SURCHG-YTD
                     TO ATNM-SURCHG-YTD
                   MOVE ATVAL-SUSPEND-FLAG
                     TO ATNM-SUSPEND-FLAG
                   MOVE ATVAL-REVIEW-FLAG
                     TO ATNM-REVIEW-FLAG
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
           DISPLAY 'ATNCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ATNCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
