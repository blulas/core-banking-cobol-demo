      *================================================================*
      * PROGRAM  : ATNRPT                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ATM NETWORK SETTLEMENT - MANAGEMENT REPORTING. READS*
      *            THE ATN TRANSACTIONS, UPDATES THE ATN MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. PROPRIETARY SHARED ATM *
      *            NETWORK SETTLEMENT AND INTERCHANGE                  *
      * INPUT    : ATNMST (KSDS), ATNTRN, BANKCTL                      *
      * OUTPUT   : ATNOUT, ATNRPRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ATVAL, ATFEE, ATWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-21 TWH  ORIGINAL                                       *
      * 1991-03-17 SKN  NEW DISPENSED REPORT COLUMNS INC5461           *
      * 1993-06-17 LMB  SURCHG CALCULATION CORRECTED CHG9204           *
      * 1994-12-28 PQR  AUDIT FINDING - CASSETTE CONTROL TOTALS CHG8365*
      * 1995-05-14 LMB  AUDIT FINDING - SETTLE CONTROL TOTALS REQ9542  *
      * 1998-01-26 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2005-08-05 BHS  NEW ADJUST REPORT COLUMNS CHG9638              *
      * 2007-12-11 LMB  NEW SURCHG REPORT COLUMNS REQ5362              *
      * 2014-06-17 MKL  INTERCHG STATUS CODES ADDED INC0538            *
      * 2014-02-09 MKL  CASSETTE EXCEPTIONS TO OPS QUEUE INC2426       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATNRPT.
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
           SELECT REPORT-FILE ASSIGN TO "ATNRPRPT"
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
                                           VALUE 20.000000.
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
      * WS-SURCHG-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURCHG-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000094000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000608500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000390500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000230500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030500000000593000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035500000000487000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035600000000629500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036100000000821500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036150000000657000'.
       01  WS-SURCHG-GRID-TBL REDEFINES WS-SURCHG-GRID-VALUES.
           05  WS-SURCHG-GRID-ROW OCCURS 10 TIMES.
               10  WS-SURCHG-GRID-LOW      PIC 9(09)V99.
               10  WS-SURCHG-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASSETTE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASSETTE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000037000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000731000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000305500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000011000'.
       01  WS-CASSETTE-BAND-TBL REDEFINES WS-CASSETTE-BAND-VALUES.
           05  WS-CASSETTE-BAND-ROW OCCURS 4 TIMES.
               10  WS-CASSETTE-BAND-LOW    PIC 9(09)V99.
               10  WS-CASSETTE-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ATNRPT  ATM NETWORK SETTLEMENT'.
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
       COPY ATFEEP.
       COPY ATWAIVP.

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
               PERFORM 3000-SPLIT-DISPENSED
               PERFORM 3010-SELECT-DISCOUNT
               PERFORM 3020-FLAG-CASSETTE
               PERFORM 3030-ACCRUE-CASSETTE
               PERFORM 3040-AGE-CLOSE
               PERFORM 3050-CHECK-EXPIRY
               PERFORM 3060-EDIT-NAME
               PERFORM 3070-ASSESS-SUSPEND
               PERFORM 3080-COUNT-NOTICE
               PERFORM 3090-ASSESS-REVIEW
               PERFORM 3100-COUNT-SEQ
               PERFORM 3110-SELECT-FEE-PCT
               PERFORM 3120-AGE-EFF
               PERFORM 3130-CHECK-EFFECTIVE
               PERFORM 3140-EDIT-ADDR
               PERFORM 3150-ASSESS-STOP
               PERFORM 3160-ACCRUE-SURCHG
               PERFORM 3170-ACCUM-STATUS
               PERFORM 3180-SELECT-RATE
               PERFORM 3190-SET-REGION
               PERFORM 3200-LIMIT-SURCHG
               PERFORM 3210-SELECT-FEE-PCT
               PERFORM 3220-AGE-NEXT-REVIEW
               PERFORM 3230-EDIT-DESC
               PERFORM 3240-COUNT-RETRY
               PERFORM 3250-SET-STATUS
               PERFORM 3260-ACCUM-TRAN
               PERFORM 3270-SPLIT-INTERCHG
               PERFORM 3280-AGE-NEXT-DUE
               PERFORM 3290-SELECT-DISCOUNT
               PERFORM 3300-PRORATE-CASSETTE
               PERFORM 3310-SELECT-RATE
               PERFORM 3320-ASSESS-EXEMPT
               PERFORM 3330-SELECT-FEE-PCT
               PERFORM 3340-DESCRIBE-REGION
               PERFORM 3350-SET-CHANNEL
               PERFORM 3360-SET-NETWORK
               PERFORM 3370-FLAG-CASSETTE
               PERFORM 3380-DESCRIBE-CHANNEL
               PERFORM 3390-ASSESS-STOP
               PERFORM 3400-CHECK-MATURITY
               PERFORM 3410-AGE-EFF
               PERFORM 3420-DESCRIBE-TERMINAL
               PERFORM 3430-SET-TERMINAL
               PERFORM 3440-SET-TERMINAL
               PERFORM 3450-PRORATE-SETTLE
               PERFORM 3460-COUNT-PAYMENT
               PERFORM 3470-ACCRUE-RATE
               PERFORM 3480-SELECT-RATE
               PERFORM 3490-ASSESS-REVERSAL
               PERFORM 3500-ACCRUE-SURCHG
               PERFORM 3510-COUNT-NOTICE
               PERFORM 3520-ACCUM-REASON
               PERFORM 3530-AGE-NOTICE
               PERFORM 3540-SELECT-FEE-PCT
               PERFORM 3550-ACCUM-STATUS
               PERFORM 3560-PRORATE-ADJUST
               PERFORM 3570-AGE-LAST-PAY
               PERFORM 3580-PRORATE-SETTLE
               PERFORM 3590-PRORATE-SURCHG
               PERFORM 3600-EDIT-CUST-ID
               PERFORM 3610-ASSESS-HOLD
               PERFORM 3620-CHECK-NEXT-REVIEW
               PERFORM 3630-DESCRIBE-BRANCH
               PERFORM 3640-ACCRUE-FEE-PCT
               PERFORM 3650-PRORATE-SURCHG
               PERFORM 3660-COUNT-SEQ
               PERFORM 3670-FLAG-CASSETTE
               PERFORM 3680-ASSESS-REVERSAL
               PERFORM 3690-LIMIT-ITEM
               PERFORM 3700-ACCUM-TERMINAL
               PERFORM 3710-EDIT-DESC
               PERFORM 3720-DESCRIBE-REGION
               PERFORM 3730-ACCUM-REGION
               PERFORM 3740-FLAG-ITEM
               PERFORM 3750-ACCUM-REASON
               PERFORM 3760-ASSESS-REVERSAL
               PERFORM 3770-DESCRIBE-NETWORK
               PERFORM 3780-ASSESS-NOTICE-SENT
               PERFORM 3790-EDIT-CONTACT
               PERFORM 3800-AGE-EFFECTIVE
               PERFORM 3810-SPLIT-SETTLE
               PERFORM 3820-DESCRIBE-STATUS
               PERFORM 3830-LIMIT-SURCHG
               PERFORM 3840-LIMIT-DISPENSED
               PERFORM 3850-FLAG-ADJUST
               PERFORM 3860-ACCRUE-DISCOUNT
               PERFORM 3870-CHECK-LAST-PAY
               PERFORM 3880-SET-CHANNEL
               PERFORM 3890-FLAG-ITEM
               PERFORM 3900-LIMIT-SETTLE
               PERFORM 3910-SET-TRAN
               PERFORM 3920-ACCUM-TERMINAL
               PERFORM 3930-SELECT-DISCOUNT
               PERFORM 3940-FLAG-CASSETTE
               PERFORM 3950-ASSESS-NOTICE-SENT
               PERFORM 3960-ASSESS-REVERSAL
               PERFORM 3970-LIMIT-SURCHG
               PERFORM 3980-SPLIT-ADJUST
               PERFORM 3990-EDIT-REF-NO
               PERFORM 4000-SELECT-RATE
               PERFORM 4010-FLAG-INTERCHG
               PERFORM 4020-EDIT-REJECT
               PERFORM 4030-SELECT-RATE
               PERFORM 4040-ASSESS-REVERSAL
               PERFORM 4050-LIMIT-ITEM
               PERFORM 4060-LIMIT-SURCHG
               PERFORM 4070-FLAG-INTERCHG
               PERFORM 4080-DESCRIBE-TRAN
               PERFORM 4090-LIMIT-SETTLE
               PERFORM 4100-SET-STATUS
               PERFORM 4110-CHECK-CLOSE
               PERFORM 4120-EDIT-DESC
               PERFORM 4130-EDIT-REF-NO
               PERFORM 4140-COUNT-PAYMENT
               PERFORM 4150-DESCRIBE-NETWORK
               PERFORM 4160-ACCRUE-CASSETTE
               PERFORM 4170-ACCUM-BRANCH
               PERFORM 4180-AGE-LAST-PAY
               PERFORM 4190-COUNT-PAYMENT
               PERFORM 4200-SET-TRAN
               PERFORM 4210-EDIT-ADDR
               PERFORM 4220-LIMIT-ITEM
               PERFORM 4230-SELECT-DISCOUNT
               PERFORM 4240-SELECT-DISCOUNT
               PERFORM 4250-DESCRIBE-TRAN
               PERFORM 4260-CHECK-CLOSE
               PERFORM 4270-FLAG-ITEM
               PERFORM 4280-FLAG-DISPENSED
               PERFORM 4290-ACCRUE-DISPENSED
               PERFORM 4300-EDIT-NAME
               PERFORM 4310-AGE-LAST-PAY
               PERFORM 4320-CHECK-NEXT-REVIEW
               PERFORM 4330-ACCRUE-DISPENSED
               PERFORM 4340-ASSESS-STOP
               PERFORM 4350-SET-TRAN
               PERFORM 4360-ACCUM-TERMINAL
               PERFORM 4370-FLAG-CASSETTE
               PERFORM 4380-COUNT-ITEM
               PERFORM 4390-SELECT-RATE
               PERFORM 4400-ASSESS-NOTICE-SENT
               PERFORM 4410-CHECK-LAST-PAY
               PERFORM 4420-CHECK-EFFECTIVE
               PERFORM 4430-EDIT-DESC
               PERFORM 4440-DESCRIBE-NETWORK
               PERFORM 4450-SELECT-RATE
               PERFORM 4460-ASSESS-REVIEW
               PERFORM 4470-ACCUM-STATUS
               PERFORM 4480-SELECT-DISCOUNT
               PERFORM 4490-AGE-NEXT-REVIEW
               PERFORM 4500-PRORATE-DISPENSED
               PERFORM 4510-FLAG-INTERCHG
               PERFORM 4520-PRORATE-INTERCHG
               PERFORM 4530-ACCUM-BRANCH
               PERFORM 4540-DESCRIBE-STATUS
               PERFORM 4550-SET-TRAN
               PERFORM 4560-ACCUM-TRAN
               PERFORM 4570-EDIT-MEMO
               PERFORM 4580-CHECK-POST
               PERFORM 4590-EDIT-MEMO
               PERFORM 4600-LIMIT-SETTLE
               PERFORM 4610-COUNT-PAYMENT
               PERFORM 4620-PRORATE-INTERCHG
               PERFORM 4630-FLAG-DISPENSED
               PERFORM 4640-SPLIT-SETTLE
               PERFORM 4650-AGE-EFFECTIVE
               PERFORM 4660-DESCRIBE-CHANNEL
               PERFORM 4670-CHECK-OPEN
               PERFORM 4680-SPLIT-SETTLE
               PERFORM 4690-EDIT-MEMO
               PERFORM 4700-FLAG-SETTLE
               PERFORM 4710-LIMIT-CASSETTE
               PERFORM 4720-AGE-LAST-PAY
               PERFORM 4730-ASSESS-STOP
               PERFORM 4740-SELECT-DISCOUNT
               PERFORM 4750-ASSESS-EXEMPT
               PERFORM 4760-COUNT-PAYMENT
               PERFORM 4770-ASSESS-NOTICE-SENT
               PERFORM 4780-FLAG-DISPENSED
               PERFORM 4790-LIMIT-SETTLE
               PERFORM 4800-PRORATE-INTERCHG
               PERFORM 4810-AGE-NEXT-DUE
               PERFORM 4820-CHECK-MATURITY
               PERFORM 4830-ACCRUE-DISPENSED
               PERFORM 4840-ACCUM-REASON
               PERFORM 7030-CALL-ATVAL
               PERFORM 7000-CALL-ATFEE
               PERFORM 7010-CALL-ATWAIV
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
      * SPLIT DISPENSED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE ATNM-INTERCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE INTERCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-INTERCHG-AMT NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3020-FLAG-CASSETTE.
           IF ATNT-CASSETTE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNT-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-CASSETTE.
           IF ATNM-CASSETTE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-CASSETTE-YTD * ATNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-CLOSE.
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
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E512 NAME REQUIRED
      * E486 SETTLE YTD NEGATIVE
      * E702 SETTLE YTD OVER 1000000
      * E559 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-YTD < 0
                   MOVE 'E486' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-YTD > 1000000.00
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE SETTLE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3070-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 10.00 TO ATNM-SURCHG-AMT
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 10.00 TO ATNM-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-SURCHG-AMT > 0 AND ATNM-SURCHG-AMT < 2.00
               MOVE 2.00 TO ATNM-SURCHG-AMT
           END-IF
           IF ATNM-SURCHG-AMT > 150.00
               MOVE 150.00 TO ATNM-SURCHG-AMT
           END-IF
           ADD ATNM-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASSETTE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3090-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ATNM-CASSETTE-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ATNM-CASSETTE-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 6
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE CASSETTE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNT-CASSETTE-AMT NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-EFF.
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
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E954 ADDR REQUIRED
      * E500 DISPENSED AMT NEGATIVE
      * E615 DISPENSED AMT OVER 10000
      * E207 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT < 0
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT > 10000.00
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE YTD FEE ON THE CASSETTE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3150-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-YTD ROUNDED = ATNT-CASSETTE-AMT
                       * ATNM-RATE / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 12.00 TO ATNM-CASSETTE-YTD
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 15.00 TO ATNM-CASSETTE-YTD
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-YTD ROUNDED = ATNT-CASSETTE-AMT
                       * ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-CASSETTE-YTD > 0 AND ATNM-CASSETTE-YTD < 5.00
               MOVE 5.00 TO ATNM-CASSETTE-YTD
           END-IF
           IF ATNM-CASSETTE-YTD > 250.00
               MOVE 250.00 TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-SURCHG.
           IF ATNT-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SURCHG-AMT * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-ADJUST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-STATUS.
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
      * SELECT THE SURCHG GRID TIER FOR THE SURCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-GRID-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNM-SURCHG-AMT NOT < WS-SURCHG-GRID-LOW(WS-TX)
                   MOVE WS-SURCHG-GRID-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN CLOSE DATE IS PAST
      * G -> H WHEN CLOSE DATE IS PAST
      * H -> P WHEN CLOSE DATE IS PAST
      * P -> V WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3200-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 0.00
               MOVE 0.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 500.00
               MOVE 500.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-DISPENSED-AMT NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-NEXT-REVIEW.
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
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E104 DESC REQUIRED
      * E783 SETTLE AMT NEGATIVE
      * E996 SETTLE AMT OVER 250000
      * E757 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 250000.00
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN ATNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 5
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN EXPIRY DATE IS PAST
      * DP -> FJ WHEN EXPIRY DATE IS PAST
      * FJ -> NA WHEN EXPIRY DATE IS PAST
      * NA -> DF WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-TRAN-CD
                   ADD ATNM-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3270-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATNM-INTERCHG-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ATNM-INTERCHG-YTD - (WS-WORK-AMT * 2)
           COMPUTE ATNM-ADJUST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-NEXT-DUE.
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
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASSETTE YTD.
      *----------------------------------------------------------------*
       3300-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-YTD ROUNDED =
                   ATNT-DISPENSED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-DISPENSED-AMT NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE DISPENSED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3320-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-DISCOUNT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 12.00 TO ATNM-DISPENSED-AMT
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 7.00 TO ATNM-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-DISPENSED-AMT > 0 AND ATNM-DISPENSED-AMT < 5.00
               MOVE 5.00 TO ATNM-DISPENSED-AMT
           END-IF
           IF ATNM-DISPENSED-AMT > 25.00
               MOVE 25.00 TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE INTERCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-INTERCHG-AMT NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'G'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
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
       3350-SET-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN NEXT REVIEW DATE IS PAST
      * C -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> S WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'C' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3370-FLAG-CASSETTE.
           IF ATNM-CASSETTE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG FEE ON THE CASSETTE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3390-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED = ATNM-CASSETTE-AMT
                       * ATNM-DISCOUNT / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 12.00 TO ATNM-INTERCHG-AMT
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 10.00 TO ATNM-INTERCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED = ATNM-CASSETTE-AMT
                       * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-INTERCHG-AMT > 0 AND ATNM-INTERCHG-AMT < 5.00
               MOVE 5.00 TO ATNM-INTERCHG-AMT
           END-IF
           IF ATNM-INTERCHG-AMT > 35.00
               MOVE 35.00 TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-MATURITY.
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
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-EFF.
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
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN OPEN DATE IS PAST
      * JJ -> JZ WHEN OPEN DATE IS PAST
      * JZ -> KK WHEN OPEN DATE IS PAST
      * KK -> BJ WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN NOTICE DATE IS PAST
      * JJ -> JZ WHEN NOTICE DATE IS PAST
      * JZ -> KK WHEN NOTICE DATE IS PAST
      * KK -> BJ WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-TERMINAL.
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
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE YTD.
      *----------------------------------------------------------------*
       3450-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-YTD ROUNDED =
                   ATNM-SURCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-SETTLE-YTD
           END-IF
           ADD ATNM-SETTLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ATNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATNM-ADJUST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-INTERCHG-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CASSETTE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3490-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNT-CASSETTE-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNT-CASSETTE-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-SURCHG.
           IF ATNT-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SURCHG-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3520-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REASON-CD
                   ADD ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE CASSETTE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-CASSETTE-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNT-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       3560-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-YTD ROUNDED =
                   ATNT-SURCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-LAST-PAY.
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
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASSETTE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3580-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-AMT ROUNDED =
                   ATNT-CASSETTE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3590-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E599 CUST ID REQUIRED
      * E291 DISPENSED AMT NEGATIVE
      * E359 DISPENSED AMT OVER 50000
      * E387 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CUST-ID = SPACES
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT < 0
                   MOVE 'E291' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT > 50000.00
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG YTD FEE ON THE SETTLE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3610-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-YTD ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 7.00 TO ATNM-INTERCHG-YTD
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 10.00 TO ATNM-INTERCHG-YTD
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-YTD ROUNDED = ATNT-SETTLE-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-INTERCHG-YTD > 0 AND ATNM-INTERCHG-YTD < 1.00
               MOVE 1.00 TO ATNM-INTERCHG-YTD
           END-IF
           IF ATNM-INTERCHG-YTD > 150.00
               MOVE 150.00 TO ATNM-INTERCHG-YTD
           END-IF
           ADD ATNM-INTERCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ATNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASSETTE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURCHG YTD.
      *----------------------------------------------------------------*
       3650-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SURCHG-YTD ROUNDED =
                   ATNM-CASSETTE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SURCHG-YTD
           END-IF
           ADD ATNM-SURCHG-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 5
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3670-FLAG-CASSETTE.
           IF ATNM-CASSETTE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST YTD FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3680-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = WS-WORK-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 15.00 TO ATNM-ADJUST-YTD
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 20.00 TO ATNM-ADJUST-YTD
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = WS-WORK-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-ADJUST-YTD > 0 AND ATNM-ADJUST-YTD < 2.00
               MOVE 2.00 TO ATNM-ADJUST-YTD
           END-IF
           IF ATNM-ADJUST-YTD > 25.00
               MOVE 25.00 TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNM-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E778 DESC REQUIRED
      * E714 CASSETTE YTD NEGATIVE
      * E341 CASSETTE YTD OVER 99999
      * E995 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD < 0
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD > 99999.00
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN ATNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E995' TO WS-REJECT-CODE
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
       3720-DESCRIBE-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISPENSED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REGION-CD
                   ADD ATNM-DISPENSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-DISPENSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3740-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-REASON.
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
      * ASSESS THE SETTLE FEE ON THE CASSETTE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3760-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 10.00
               MOVE 10.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 25.00
               MOVE 25.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE SETTLE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3780-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-REASON-CD = 'AK'
                   MOVE 10.00 TO ATNM-SETTLE-AMT
               WHEN ATNM-REASON-CD = 'HX'
                   MOVE 10.00 TO ATNM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-SETTLE-AMT > 0 AND ATNM-SETTLE-AMT < 2.00
               MOVE 2.00 TO ATNM-SETTLE-AMT
           END-IF
           IF ATNM-SETTLE-AMT > 150.00
               MOVE 150.00 TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E487 CONTACT REQUIRED
      * E569 SURCHG YTD NEGATIVE
      * E771 SURCHG YTD OVER 1000000
      * E405 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD < 0
                   MOVE 'E569' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD > 1000000.00
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E405' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNM-SETTLE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-SETTLE-AMT - (WS-WORK-AMT * 12)
           COMPUTE ATNM-ADJUST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-SURCHG.
           IF ATNT-SURCHG-AMT < 100.00
               MOVE 100.00 TO ATNT-SURCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-SURCHG-AMT > 50000.00
               MOVE 50000.00 TO ATNT-SURCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-DISPENSED.
           IF ATNM-DISPENSED-YTD < 10.00
               MOVE 10.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-DISPENSED-YTD > 5000.00
               MOVE 5000.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3850-FLAG-ADJUST.
           IF ATNM-ADJUST-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-ADJUST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-LAST-PAY.
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
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3890-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-SETTLE.
           IF ATNM-SETTLE-YTD < 25.00
               MOVE 25.00 TO ATNM-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SETTLE-YTD > 50000.00
               MOVE 50000.00 TO ATNM-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN EFFECTIVE DATE IS PAST
      * BI -> TR WHEN EFFECTIVE DATE IS PAST
      * TR -> AD WHEN EFFECTIVE DATE IS PAST
      * AD -> RV WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG YTD BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNM-SURCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SURCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3940-FLAG-CASSETTE.
           IF ATNT-CASSETTE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNT-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CASSETTE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3950-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNT-CASSETTE-AMT *
                       ATNM-RATE / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNT-CASSETTE-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE SURCHG AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3960-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ATNM-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED = ATNM-SURCHG-AMT
                       * ATNM-FEE-PCT / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 7.00 TO ATNM-DISPENSED-AMT
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 5.00 TO ATNM-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED = ATNM-SURCHG-AMT
                       * ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-DISPENSED-AMT > 0 AND ATNM-DISPENSED-AMT < 2.00
               MOVE 2.00 TO ATNM-DISPENSED-AMT
           END-IF
           IF ATNM-DISPENSED-AMT > 50.00
               MOVE 50.00 TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SURCHG AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3970-LIMIT-SURCHG.
           IF ATNM-SURCHG-AMT < 100.00
               MOVE 100.00 TO ATNM-SURCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-AMT > 2500.00
               MOVE 2500.00 TO ATNM-SURCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNM-DISPENSED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E552 REF NO REQUIRED
      * E296 ADJUST YTD NEGATIVE
      * E024 ADJUST YTD OVER 25000
      * E390 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-REF-NO = SPACES
                   MOVE 'E552' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-YTD < 0
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-YTD > 25000.00
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN ATNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG GRID TIER FOR THE SURCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-GRID-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNT-SURCHG-AMT NOT < WS-SURCHG-GRID-LOW(WS-TX)
                   MOVE WS-SURCHG-GRID-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4010-FLAG-INTERCHG.
           IF ATNM-INTERCHG-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-INTERCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E682 REJECT CD REQUIRED
      * E277 ADJUST AMT NEGATIVE
      * E546 ADJUST AMT OVER 10000
      * E049 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-REJECT-CD = SPACES
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT < 0
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT > 10000.00
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN ATNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E049' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-DISPENSED-AMT NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG YTD FEE ON THE SURCHG AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4040-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-YTD ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 15.00 TO ATNM-SURCHG-YTD
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 12.00 TO ATNM-SURCHG-YTD
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-YTD ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-SURCHG-YTD > 0 AND ATNM-SURCHG-YTD < 5.00
               MOVE 5.00 TO ATNM-SURCHG-YTD
           END-IF
           IF ATNM-SURCHG-YTD > 35.00
               MOVE 35.00 TO ATNM-SURCHG-YTD
           END-IF
           ADD ATNM-SURCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 50.00
               MOVE 50.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 50000.00
               MOVE 50000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4070-FLAG-INTERCHG.
           IF ATNM-INTERCHG-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-INTERCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BI'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4090-LIMIT-SETTLE.
           IF ATNT-SETTLE-AMT < 25.00
               MOVE 25.00 TO ATNT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-SETTLE-AMT > 500.00
               MOVE 500.00 TO ATNT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN EFF DATE IS PAST
      * DP -> FJ WHEN EFF DATE IS PAST
      * FJ -> NA WHEN EFF DATE IS PAST
      * NA -> DF WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-STATUS.
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
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-CLOSE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E729 DESC REQUIRED
      * E744 CASSETTE YTD NEGATIVE
      * E365 CASSETTE YTD OVER 250000
      * E656 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E729' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD < 0
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-YTD > 250000.00
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN ATNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E656' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E514 REF NO REQUIRED
      * E030 INTERCHG AMT NEGATIVE
      * E340 INTERCHG AMT OVER 10000
      * E113 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-REF-NO = SPACES
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN ATNM-INTERCHG-AMT < 0
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN ATNM-INTERCHG-AMT > 10000.00
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4160-ACCRUE-CASSETTE.
           IF ATNT-CASSETTE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-CASSETTE-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISPENSED YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNM-DISPENSED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-DISPENSED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-LAST-PAY.
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
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4190-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN LAST PAY DATE IS PAST
      * BI -> TR WHEN LAST PAY DATE IS PAST
      * TR -> AD WHEN LAST PAY DATE IS PAST
      * AD -> RV WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E748 ADDR REQUIRED
      * E635 ADJUST AMT NEGATIVE
      * E936 ADJUST AMT OVER 99999
      * E359 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E748' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT < 0
                   MOVE 'E635' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT > 99999.00
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN ATNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG GRID TIER FOR THE CASSETTE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-GRID-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNT-CASSETTE-AMT NOT < WS-SURCHG-GRID-LOW(WS-TX)
                   MOVE WS-SURCHG-GRID-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE SURCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-SURCHG-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BI'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-CLOSE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4270-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4280-FLAG-DISPENSED.
           IF ATNM-DISPENSED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-AMT * ATNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E276 NAME REQUIRED
      * E542 DISPENSED AMT NEGATIVE
      * E478 DISPENSED AMT OVER 1000000
      * E443 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT < 0
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT > 1000000.00
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN ATNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E443' TO WS-REJECT-CODE
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
       4310-AGE-LAST-PAY.
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
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4320-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4330-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE ADJUST YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4340-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-ADJUST-YTD *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 12.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-ADJUST-YTD *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 1.00
               MOVE 1.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 50.00
               MOVE 50.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN MATURITY DATE IS PAST
      * BI -> TR WHEN MATURITY DATE IS PAST
      * TR -> AD WHEN MATURITY DATE IS PAST
      * AD -> RV WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4350-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4370-FLAG-CASSETTE.
           IF ATNM-CASSETTE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4380-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 12
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE CASSETTE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-CASSETTE-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE DISPENSED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4400-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-DISPENSED-YTD
                       * ATNM-FEE-PCT / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-DISPENSED-YTD
                       * ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 2.00
               MOVE 2.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 150.00
               MOVE 150.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4410-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4420-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E876 DESC REQUIRED
      * E717 DISPENSED YTD NEGATIVE
      * E633 DISPENSED YTD OVER 99999
      * E531 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4430-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-YTD < 0
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-YTD > 99999.00
                   MOVE 'E633' TO WS-REJECT-CODE
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
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4440-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4450-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-SETTLE-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE CASSETTE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4460-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED = ATNM-CASSETTE-YTD
                       * ATNM-DISCOUNT / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 7.00 TO ATNM-CASSETTE-AMT
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 3.00 TO ATNM-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED = ATNM-CASSETTE-YTD
                       * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-CASSETTE-AMT > 0 AND ATNM-CASSETTE-AMT < 2.00
               MOVE 2.00 TO ATNM-CASSETTE-AMT
           END-IF
           IF ATNM-CASSETTE-AMT > 35.00
               MOVE 35.00 TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4470-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNM-SURCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SURCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG GRID TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4480-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-GRID-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNM-SETTLE-YTD NOT < WS-SURCHG-GRID-LOW(WS-TX)
                   MOVE WS-SURCHG-GRID-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4490-AGE-NEXT-REVIEW.
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
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISPENSED AMT.
      *----------------------------------------------------------------*
       4500-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-DISPENSED-AMT ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNT-DISPENSED-AMT
           END-IF
           ADD ATNT-DISPENSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INTERCHG YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4510-FLAG-INTERCHG.
           IF ATNM-INTERCHG-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-INTERCHG-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4520-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-INTERCHG-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4530-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNM-INTERCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-INTERCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4540-DESCRIBE-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN EFF DATE IS PAST
      * BI -> TR WHEN EFF DATE IS PAST
      * TR -> AD WHEN EFF DATE IS PAST
      * AD -> RV WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4550-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4560-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-TRAN-CD
                   ADD ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E684 MEMO REQUIRED
      * E963 SETTLE AMT NEGATIVE
      * E417 SETTLE AMT OVER 50000
      * E512 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4570-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-MEMO = SPACES
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 50000.00
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4580-CHECK-POST.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E817 MEMO REQUIRED
      * E388 SURCHG YTD NEGATIVE
      * E356 SURCHG YTD OVER 10000
      * E686 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4590-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-MEMO = SPACES
                   MOVE 'E817' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD < 0
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD > 10000.00
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN ATNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4600-LIMIT-SETTLE.
           IF ATNM-SETTLE-YTD < 50.00
               MOVE 50.00 TO ATNM-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SETTLE-YTD > 1000.00
               MOVE 1000.00 TO ATNM-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4610-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTERCHG AMT.
      *----------------------------------------------------------------*
       4620-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-INTERCHG-AMT ROUNDED =
                   ATNM-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4630-FLAG-DISPENSED.
           IF ATNM-DISPENSED-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4640-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNT-SETTLE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ATNT-SETTLE-AMT - (WS-WORK-AMT * 4)
           COMPUTE ATNM-SURCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4650-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4660-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4670-CHECK-OPEN.
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
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4680-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNM-SETTLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SETTLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNT-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E948 MEMO REQUIRED
      * E535 DISPENSED AMT NEGATIVE
      * E003 DISPENSED AMT OVER 50000
      * E427 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4690-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-MEMO = SPACES
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT < 0
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT > 50000.00
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN ATNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4700-FLAG-SETTLE.
           IF ATNT-SETTLE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNT-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4710-LIMIT-CASSETTE.
           IF ATNM-CASSETTE-YTD < 0.00
               MOVE 0.00 TO ATNM-CASSETTE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-CASSETTE-YTD > 5000.00
               MOVE 5000.00 TO ATNM-CASSETTE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4720-AGE-LAST-PAY.
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
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG FEE ON THE SURCHG AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4730-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 10.00 TO ATNM-INTERCHG-AMT
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 12.00 TO ATNM-INTERCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-INTERCHG-AMT > 0 AND ATNM-INTERCHG-AMT < 10.00
               MOVE 10.00 TO ATNM-INTERCHG-AMT
           END-IF
           IF ATNM-INTERCHG-AMT > 25.00
               MOVE 25.00 TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE DISPENSED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4740-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-DISPENSED-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE DISPENSED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4750-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-DISPENSED-YTD
                       * ATNM-RATE / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 5.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 5.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-DISPENSED-YTD
                       * ATNM-RATE / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 2.00
               MOVE 2.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 25.00
               MOVE 25.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4760-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE CASSETTE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4770-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-SETTLE-YTD ROUNDED = ATNT-CASSETTE-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 7.00 TO ATNM-SETTLE-YTD
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 7.00 TO ATNM-SETTLE-YTD
               WHEN OTHER
                   COMPUTE ATNM-SETTLE-YTD ROUNDED = ATNT-CASSETTE-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-SETTLE-YTD > 0 AND ATNM-SETTLE-YTD < 1.00
               MOVE 1.00 TO ATNM-SETTLE-YTD
           END-IF
           IF ATNM-SETTLE-YTD > 150.00
               MOVE 150.00 TO ATNM-SETTLE-YTD
           END-IF
           ADD ATNM-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISPENSED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4780-FLAG-DISPENSED.
           IF ATNT-DISPENSED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNT-DISPENSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4790-LIMIT-SETTLE.
           IF ATNM-SETTLE-YTD < 100.00
               MOVE 100.00 TO ATNM-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SETTLE-YTD > 2500.00
               MOVE 2500.00 TO ATNM-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTERCHG AMT.
      *----------------------------------------------------------------*
       4800-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-INTERCHG-AMT ROUNDED =
                   ATNM-DISPENSED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4810-AGE-NEXT-DUE.
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
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4820-CHECK-MATURITY.
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
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4830-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4840-ACCUM-REASON.
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
                   MOVE ATVAL-INTERCHG-YTD
                     TO ATNM-INTERCHG-YTD
                   MOVE ATVAL-CASSETTE-AMT
                     TO ATNM-CASSETTE-AMT
                   MOVE ATVAL-SETTLE-YTD
                     TO ATNM-SETTLE-YTD
                   MOVE ATVAL-SURCHG-AMT
                     TO ATNM-SURCHG-AMT
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

      *----------------------------------------------------------------*
      * PASS THE ATN FIELDS TO ATFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-ATFEE.
           INITIALIZE ATFEE-PARMS
           MOVE ATNM-DISPENSED-YTD
             TO ATFEE-DISPENSED-YTD
           MOVE ATNM-SETTLE-YTD
             TO ATFEE-SETTLE-YTD
           MOVE ATNM-ADJUST-YTD
             TO ATFEE-ADJUST-YTD
           MOVE ATNM-CASSETTE-AMT
             TO ATFEE-CASSETTE-AMT
           MOVE ATNM-SETTLE-AMT
             TO ATFEE-SETTLE-AMT
           MOVE ATNM-INTERCHG-YTD
             TO ATFEE-INTERCHG-YTD
           MOVE ATNM-OPEN-DATE
             TO ATFEE-OPEN-DATE
           MOVE ATNM-MATURITY-DATE
             TO ATFEE-MATURITY-DATE
           MOVE ATNM-NOTICE-DATE
             TO ATFEE-NOTICE-DATE
           MOVE ATNM-LAST-PAY-DATE
             TO ATFEE-LAST-PAY-DATE
           MOVE ATNM-STATUS-CD
             TO ATFEE-STATUS-CD
           MOVE ATNM-REGION-CD
             TO ATFEE-REGION-CD
           MOVE ATNM-TERMINAL-CD
             TO ATFEE-TERMINAL-CD
           MOVE ATNM-BRANCH-CD
             TO ATFEE-BRANCH-CD
           MOVE ATNM-REVIEW-FLAG
             TO ATFEE-REVIEW-FLAG
           MOVE ATNM-STOP-FLAG
             TO ATFEE-STOP-FLAG
           MOVE ATNM-EXEMPT-FLAG
             TO ATFEE-EXEMPT-FLAG
           MOVE ATNM-RETRY-CNT
             TO ATFEE-RETRY-CNT
           MOVE ATNM-NOTICE-CNT
             TO ATFEE-NOTICE-CNT
           MOVE ATNM-DISCOUNT
             TO ATFEE-DISCOUNT
           MOVE ATNM-FEE-PCT
             TO ATFEE-FEE-PCT
           MOVE ATNM-ADDR
             TO ATFEE-ADDR
           MOVE ATNM-REF-NO
             TO ATFEE-REF-NO
           CALL 'ATFEE' USING ATFEE-PARMS
           EVALUATE ATFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ATFEE-CASSETTE-AMT
                     TO ATNM-CASSETTE-AMT
                   MOVE ATFEE-INTERCHG-YTD
                     TO ATNM-INTERCHG-YTD
                   MOVE ATFEE-REVIEW-FLAG
                     TO ATNM-REVIEW-FLAG
                   MOVE ATFEE-STOP-FLAG
                     TO ATNM-STOP-FLAG
                   MOVE ATFEE-EXEMPT-FLAG
                     TO ATNM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ATFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ATN FIELDS TO ATWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-ATWAIV.
           INITIALIZE ATWAIV-PARMS
           MOVE ATNM-INTERCHG-YTD
             TO ATWAIV-INTERCHG-YTD
           MOVE ATNM-SETTLE-AMT
             TO ATWAIV-SETTLE-AMT
           MOVE ATNM-ADJUST-YTD
             TO ATWAIV-ADJUST-YTD
           MOVE ATNM-DISPENSED-YTD
             TO ATWAIV-DISPENSED-YTD
           MOVE ATNM-ADJUST-AMT
             TO ATWAIV-ADJUST-AMT
           MOVE ATNM-SETTLE-YTD
             TO ATWAIV-SETTLE-YTD
           MOVE ATNM-NEXT-REVIEW-DATE
             TO ATWAIV-NEXT-REVIEW-DATE
           MOVE ATNM-MATURITY-DATE
             TO ATWAIV-MATURITY-DATE
           MOVE ATNM-LAST-PAY-DATE
             TO ATWAIV-LAST-PAY-DATE
           MOVE ATNM-EFFECTIVE-DATE
             TO ATWAIV-EFFECTIVE-DATE
           MOVE ATNM-BRANCH-CD
             TO ATWAIV-BRANCH-CD
           MOVE ATNM-TERMINAL-CD
             TO ATWAIV-TERMINAL-CD
           MOVE ATNM-NETWORK-CD
             TO ATWAIV-NETWORK-CD
           MOVE ATNM-STATUS-CD
             TO ATWAIV-STATUS-CD
           MOVE ATNM-SUSPEND-FLAG
             TO ATWAIV-SUSPEND-FLAG
           MOVE ATNM-NOTICE-SENT-FLAG
             TO ATWAIV-NOTICE-SENT-FLAG
           MOVE ATNM-REVIEW-FLAG
             TO ATWAIV-REVIEW-FLAG
           MOVE ATNM-PAYMENT-CNT
             TO ATWAIV-PAYMENT-CNT
           MOVE ATNM-ITEM-CNT
             TO ATWAIV-ITEM-CNT
           MOVE ATNM-FEE-PCT
             TO ATWAIV-FEE-PCT
           MOVE ATNM-RATE
             TO ATWAIV-RATE
           MOVE ATNM-DESC
             TO ATWAIV-DESC
           MOVE ATNM-NAME
             TO ATWAIV-NAME
           CALL 'ATWAIV' USING ATWAIV-PARMS
           EVALUATE ATWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ATWAIV-ADJUST-YTD
                     TO ATNM-ADJUST-YTD
                   MOVE ATWAIV-ADJUST-AMT
                     TO ATNM-ADJUST-AMT
                   MOVE ATWAIV-SUSPEND-FLAG
                     TO ATNM-SUSPEND-FLAG
                   MOVE ATWAIV-NOTICE-SENT-FLAG
                     TO ATNM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-ATWAIV' TO ABND-PARAGRAPH
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
           DISPLAY 'ATNRPT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ATNRPT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
