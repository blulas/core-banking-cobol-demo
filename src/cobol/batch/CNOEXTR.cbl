      *================================================================*
      * PROGRAM  : CNOEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COIN AND CURRENCY - NIGHTLY EXTRACT. READS THE CCO  *
      *            TRANSACTIONS, UPDATES THE CCO MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BRANCH COIN AND CURRENCY ORDERS *
      *            AND ARMORED CARRIER SHIPMENT                        *
      * INPUT    : CCOMST (KSDS), CCOTRN, BANKCTL                      *
      * OUTPUT   : CCOOUT, CNOEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CNCAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-07-02 MKL  ORIGINAL                                       *
      * 1997-02-19 PQR  SHIPMENT EXCEPTIONS TO OPS QUEUE REQ1298       *
      * 1998-01-19 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-04 BHS  AUDIT FINDING - SHIPMENT CONTROL TOTALS INC9117*
      * 1999-12-09 BHS  STRAP LIMIT REVISED INC8778                    *
      * 2001-05-03 TWH  CURRENCY RERUN PROTECTION INC2680              *
      * 2004-11-13 TWH  VARIANCE ROUNDING - HALF UP INC8012            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNOEXTR.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "CCOMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CCOM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "CCOTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "CCOOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CNOEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY CCOMST.
       FD  TRAN-FILE.
       COPY CCOTRN.
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
                                           VALUE 21.000000.
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
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000082000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000092500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000504000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000877000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000100000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000223500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007900000000173000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008400000000345000'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 8 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000904500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000156000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000153500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000143500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000683000'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 5 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CNOEXTR  COIN AND CURRENCY'.
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
       COPY CNCALP.

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
               MOVE 'CCOMST' TO ABND-FILE-NAME
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
           MOVE CCOT-KEY TO CCOM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-PRORATE-COIN
               PERFORM 3010-COUNT-SEQ
               PERFORM 3020-SELECT-PENALTY-PCT
               PERFORM 3030-COUNT-SEQ
               PERFORM 3040-SET-REASON
               PERFORM 3050-LIMIT-CURRENCY
               PERFORM 3060-LIMIT-COIN
               PERFORM 3070-COUNT-CYCLE
               PERFORM 3080-EDIT-CUST-ID
               PERFORM 3090-AGE-LAST-ACT
               PERFORM 3100-CHECK-POST
               PERFORM 3110-ACCRUE-VARIANCE
               PERFORM 3120-ASSESS-AUTO-RENEW
               PERFORM 3130-FLAG-CURRENCY
               PERFORM 3140-ACCRUE-RATE
               PERFORM 3150-SELECT-PENALTY-PCT
               PERFORM 3160-ACCUM-CHANNEL
               PERFORM 3170-FLAG-VARIANCE
               PERFORM 3180-LIMIT-ITEM
               PERFORM 3190-FLAG-CURRENCY
               PERFORM 3200-ACCUM-CHANNEL
               PERFORM 3210-CHECK-LAST-PAY
               PERFORM 3220-SELECT-PENALTY-PCT
               PERFORM 3230-LIMIT-COIN
               PERFORM 3240-SELECT-RATE
               PERFORM 3250-CHECK-NEXT-DUE
               PERFORM 3260-LIMIT-STRAP
               PERFORM 3270-EDIT-REF-NO
               PERFORM 3280-SELECT-FEE-PCT
               PERFORM 3290-AGE-OPEN
               PERFORM 3300-DESCRIBE-CHANNEL
               PERFORM 3310-AGE-NEXT-REVIEW
               PERFORM 3320-SELECT-RATE
               PERFORM 3330-SPLIT-STRAP
               PERFORM 3340-ACCUM-DENOM
               PERFORM 3350-SPLIT-SHIPMENT
               PERFORM 3360-SET-SOURCE
               PERFORM 3370-AGE-EFF
               PERFORM 3380-FLAG-SHIPMENT
               PERFORM 3390-ACCRUE-PENALTY-PCT
               PERFORM 3400-COUNT-RETRY
               PERFORM 3410-ACCRUE-STRAP
               PERFORM 3420-FLAG-FEE
               PERFORM 3430-PRORATE-VARIANCE
               PERFORM 3440-ACCRUE-FEE
               PERFORM 3450-LIMIT-VARIANCE
               PERFORM 3460-ACCRUE-SHIPMENT
               PERFORM 3470-ASSESS-OVERRIDE
               PERFORM 3480-ACCRUE-RATE
               PERFORM 3490-PRORATE-SHIPMENT
               PERFORM 3500-CHECK-MATURITY
               PERFORM 3510-CHECK-POST
               PERFORM 3520-DESCRIBE-DENOM
               PERFORM 3530-PRORATE-VARIANCE
               PERFORM 3540-CHECK-LAST-ACT
               PERFORM 3550-ASSESS-HOLD
               PERFORM 3560-COUNT-NOTICE
               PERFORM 3570-SELECT-PENALTY-PCT
               PERFORM 3580-SPLIT-VARIANCE
               PERFORM 3590-COUNT-RETRY
               PERFORM 3600-ACCRUE-STRAP
               PERFORM 3610-CHECK-EFFECTIVE
               PERFORM 3620-CHECK-MATURITY
               PERFORM 3630-SELECT-FEE-PCT
               PERFORM 3640-CHECK-LAST-ACT
               PERFORM 3650-ACCUM-CHANNEL
               PERFORM 3660-FLAG-VARIANCE
               PERFORM 3670-ASSESS-EXEMPT
               PERFORM 3680-ASSESS-STOP
               PERFORM 3690-LIMIT-VARIANCE
               PERFORM 3700-CHECK-MATURITY
               PERFORM 3710-ACCRUE-SHIPMENT
               PERFORM 3720-DESCRIBE-CHANNEL
               PERFORM 3730-PRORATE-COIN
               PERFORM 3740-DESCRIBE-CARRIER
               PERFORM 3750-PRORATE-STRAP
               PERFORM 3760-SPLIT-VARIANCE
               PERFORM 3770-FLAG-VARIANCE
               PERFORM 3780-EDIT-ADDR
               PERFORM 3790-PRORATE-STRAP
               PERFORM 3800-EDIT-REJECT
               PERFORM 3810-LIMIT-COIN
               PERFORM 3820-ACCRUE-STRAP
               PERFORM 3830-ASSESS-AUTO-RENEW
               PERFORM 3840-PRORATE-STRAP
               PERFORM 3850-AGE-POST
               PERFORM 3860-PRORATE-VARIANCE
               PERFORM 3870-FLAG-CURRENCY
               PERFORM 3880-ACCUM-CARRIER
               PERFORM 3890-ASSESS-AUTO-RENEW
               PERFORM 3900-LIMIT-COIN
               PERFORM 3910-SET-CARRIER
               PERFORM 3920-SPLIT-VARIANCE
               PERFORM 3930-FLAG-ITEM
               PERFORM 3940-COUNT-CYCLE
               PERFORM 3950-ACCUM-DENOM
               PERFORM 3960-EDIT-CONTACT
               PERFORM 3970-ACCRUE-SHIPMENT
               PERFORM 3980-ACCRUE-VARIANCE
               PERFORM 3990-EDIT-MEMO
               PERFORM 4000-ASSESS-AUTO-RENEW
               PERFORM 4010-SPLIT-CURRENCY
               PERFORM 4020-CHECK-POST
               PERFORM 4030-SET-TRAN
               PERFORM 4040-PRORATE-STRAP
               PERFORM 4050-SELECT-RATE
               PERFORM 4060-ACCUM-TRAN
               PERFORM 4070-ASSESS-AUTO-RENEW
               PERFORM 4080-EDIT-CUST-ID
               PERFORM 4090-ACCUM-DENOM
               PERFORM 7020-CALL-CNCAL
               REWRITE CCO-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'CCOMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM CCO-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-COIN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 6
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 5
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN NOTICE DATE IS PAST
      * LW -> PS WHEN NOTICE DATE IS PAST
      * PS -> JL WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CURRENCY AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-CURRENCY.
           IF CCOM-CURRENCY-AMT < 0.00
               MOVE 0.00 TO CCOM-CURRENCY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-CURRENCY-AMT > 10000.00
               MOVE 10000.00 TO CCOM-CURRENCY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-COIN.
           IF CCOT-COIN-AMT < 25.00
               MOVE 25.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 50000.00
               MOVE 50000.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E022 CUST ID REQUIRED
      * E010 COIN AMT NEGATIVE
      * E523 COIN AMT OVER 25000
      * E899 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN CCOM-COIN-AMT < 0
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN CCOM-COIN-AMT > 25000.00
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-VARIANCE.
           IF CCOM-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOM-COIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP FEE ON THE VARIANCE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3120-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-AMT ROUNDED = CCOT-VARIANCE-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 10.00 TO CCOM-STRAP-AMT
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 15.00 TO CCOM-STRAP-AMT
               WHEN OTHER
                   COMPUTE CCOM-STRAP-AMT ROUNDED = CCOT-VARIANCE-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-STRAP-AMT > 0 AND CCOM-STRAP-AMT < 2.00
               MOVE 2.00 TO CCOM-STRAP-AMT
           END-IF
           IF CCOM-STRAP-AMT > 150.00
               MOVE 150.00 TO CCOM-STRAP-AMT
           END-IF
           ADD CCOM-STRAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CURRENCY AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3130-FLAG-CURRENCY.
           IF CCOM-CURRENCY-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOM-STRAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3170-FLAG-VARIANCE.
           IF CCOM-VARIANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-VARIANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3190-FLAG-CURRENCY.
           IF CCOM-CURRENCY-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CHANNEL-CD
                   ADD CCOM-STRAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CCOM-FEE-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3230-LIMIT-COIN.
           IF CCOM-COIN-AMT < 50.00
               MOVE 50.00 TO CCOM-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-COIN-AMT > 2500.00
               MOVE 2500.00 TO CCOM-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CCOM-STRAP-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-STRAP.
           IF CCOT-STRAP-AMT < 100.00
               MOVE 100.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-STRAP-AMT > 10000.00
               MOVE 10000.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E673 REF NO REQUIRED
      * E275 SHIPMENT AMT NEGATIVE
      * E735 SHIPMENT AMT OVER 250000
      * E881 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-REF-NO = SPACES
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-AMT < 0
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-AMT > 250000.00
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN CCOM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CCOM-COIN-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-REVIEW-DATE > 0 AND CCOM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE SHIPMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOM-SHIPMENT-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3330-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOM-STRAP-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-STRAP-YTD - (WS-WORK-AMT * 2)
           COMPUTE CCOM-CURRENCY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3340-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-SHIPMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-SHIPMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE CCOT-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CFR -> DTX WHEN OPEN DATE IS PAST
      * DTX -> EEA WHEN OPEN DATE IS PAST
      * EEA -> EUA WHEN OPEN DATE IS PAST
      * EUA -> TPB WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DTX' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DTX'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EEA' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EEA'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EUA'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TPB' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-SHIPMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3380-FLAG-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOT-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-CURRENCY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-STRAP.
           IF CCOM-STRAP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-STRAP-YTD * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3420-FLAG-FEE.
           IF CCOM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VARIANCE AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-AMT ROUNDED =
                   CCOM-COIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-FEE.
           IF CCOM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-FEE-AMT * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOT-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3450-LIMIT-VARIANCE.
           IF CCOT-VARIANCE-AMT < 100.00
               MOVE 100.00 TO CCOT-VARIANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-VARIANCE-AMT > 10000.00
               MOVE 10000.00 TO CCOT-VARIANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3470-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOT-COIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-SHIPMENT-AMT ROUNDED =
                   CCOM-SHIPMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3510-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3530-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CCOM-VARIANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE FEE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3550-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-FEE-YTD *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 3.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 5.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-FEE-YTD *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 2.00
               MOVE 2.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 100.00
               MOVE 100.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOT-COIN-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-AMT - (WS-WORK-AMT * 4)
           COMPUTE CCOM-STRAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 12
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-STRAP.
           IF CCOM-STRAP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-STRAP-YTD * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-CURRENCY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3630-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3640-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CHANNEL-CD
                   ADD CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3660-FLAG-VARIANCE.
           IF CCOM-VARIANCE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-VARIANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP FEE ON THE CURRENCY YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3670-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-AMT ROUNDED = CCOM-CURRENCY-YTD *
                       CCOM-RATE / 200
               WHEN CCOM-REASON-CD = 'JL'
                   MOVE 7.00 TO CCOM-STRAP-AMT
               WHEN CCOM-REASON-CD = 'LW'
                   MOVE 5.00 TO CCOM-STRAP-AMT
               WHEN OTHER
                   COMPUTE CCOM-STRAP-AMT ROUNDED = CCOM-CURRENCY-YTD *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-STRAP-AMT > 0 AND CCOM-STRAP-AMT < 10.00
               MOVE 10.00 TO CCOM-STRAP-AMT
           END-IF
           IF CCOM-STRAP-AMT > 35.00
               MOVE 35.00 TO CCOM-STRAP-AMT
           END-IF
           ADD CCOM-STRAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE YTD FEE ON THE STRAP YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3680-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-STRAP-YTD *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 10.00 TO CCOM-VARIANCE-YTD
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 3.00 TO CCOM-VARIANCE-YTD
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-STRAP-YTD *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-VARIANCE-YTD > 0 AND CCOM-VARIANCE-YTD < 5.00
               MOVE 5.00 TO CCOM-VARIANCE-YTD
           END-IF
           IF CCOM-VARIANCE-YTD > 35.00
               MOVE 35.00 TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * VARIANCE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3690-LIMIT-VARIANCE.
           IF CCOT-VARIANCE-AMT < 10.00
               MOVE 10.00 TO CCOT-VARIANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-VARIANCE-AMT > 2500.00
               MOVE 2500.00 TO CCOT-VARIANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-STRAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CURRENCY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COIN AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-COIN-AMT ROUNDED =
                   CCOM-CURRENCY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOT-COIN-AMT
           END-IF
           ADD CCOT-COIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3740-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STRAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STRAP AMT.
      *----------------------------------------------------------------*
       3750-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-STRAP-AMT ROUNDED =
                   CCOM-STRAP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOT-STRAP-AMT
           END-IF
           ADD CCOT-STRAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CCOM-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3770-FLAG-VARIANCE.
           IF CCOM-VARIANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-VARIANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E904 ADDR REQUIRED
      * E307 CURRENCY YTD NEGATIVE
      * E918 CURRENCY YTD OVER 5000
      * E473 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-ADDR = SPACES
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD < 0
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD > 5000.00
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN CCOM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STRAP YTD.
      *----------------------------------------------------------------*
       3790-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-STRAP-YTD ROUNDED =
                   CCOM-SHIPMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E011 REJECT CD REQUIRED
      * E375 WORK AMT NEGATIVE
      * E468 WORK AMT OVER 250000
      * E968 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-REJECT-CD = SPACES
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-COIN.
           IF CCOT-COIN-AMT < 50.00
               MOVE 50.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 5000.00
               MOVE 5000.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-STRAP.
           IF CCOM-STRAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-STRAP-AMT * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-CURRENCY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP YTD FEE ON THE FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3830-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-YTD ROUNDED = CCOM-FEE-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 10.00 TO CCOM-STRAP-YTD
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 15.00 TO CCOM-STRAP-YTD
               WHEN OTHER
                   COMPUTE CCOM-STRAP-YTD ROUNDED = CCOM-FEE-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-STRAP-YTD > 0 AND CCOM-STRAP-YTD < 1.00
               MOVE 1.00 TO CCOM-STRAP-YTD
           END-IF
           IF CCOM-STRAP-YTD > 150.00
               MOVE 150.00 TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STRAP AMT.
      *----------------------------------------------------------------*
       3840-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-STRAP-AMT ROUNDED =
                   CCOM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-STRAP-AMT
           END-IF
           ADD CCOM-STRAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VARIANCE AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CURRENCY AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3870-FLAG-CURRENCY.
           IF CCOM-CURRENCY-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN FEE ON THE STRAP AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3890-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOM-COIN-AMT ROUNDED = CCOT-STRAP-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 10.00 TO CCOM-COIN-AMT
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 7.00 TO CCOM-COIN-AMT
               WHEN OTHER
                   COMPUTE CCOM-COIN-AMT ROUNDED = CCOT-STRAP-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-COIN-AMT > 0 AND CCOM-COIN-AMT < 5.00
               MOVE 5.00 TO CCOM-COIN-AMT
           END-IF
           IF CCOM-COIN-AMT > 250.00
               MOVE 250.00 TO CCOM-COIN-AMT
           END-IF
           ADD CCOM-COIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-COIN.
           IF CCOT-COIN-AMT < 25.00
               MOVE 25.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 2500.00
               MOVE 2500.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN EXPIRY DATE IS PAST
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> X WHEN EXPIRY DATE IS PAST
      * X -> J WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOT-VARIANCE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOT-VARIANCE-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3930-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-COIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-COIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E289 CONTACT REQUIRED
      * E267 CURRENCY AMT NEGATIVE
      * E516 CURRENCY AMT OVER 10000
      * E213 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT > 10000.00
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-SHIPMENT.
           IF CCOM-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-STRAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-VARIANCE.
           IF CCOT-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E528 MEMO REQUIRED
      * E751 CURRENCY YTD NEGATIVE
      * E699 CURRENCY YTD OVER 5000
      * E558 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-MEMO = SPACES
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD < 0
                   MOVE 'E751' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD > 5000.00
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E558' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4000-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOT-STRAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOT-STRAP-AMT ROUNDED = CCOM-SHIPMENT-YTD *
                       CCOM-RATE / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 10.00 TO CCOT-STRAP-AMT
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 5.00 TO CCOT-STRAP-AMT
               WHEN OTHER
                   COMPUTE CCOT-STRAP-AMT ROUNDED = CCOM-SHIPMENT-YTD *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOT-STRAP-AMT > 0 AND CCOT-STRAP-AMT < 5.00
               MOVE 5.00 TO CCOT-STRAP-AMT
           END-IF
           IF CCOT-STRAP-AMT > 150.00
               MOVE 150.00 TO CCOT-STRAP-AMT
           END-IF
           ADD CCOT-STRAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOM-CURRENCY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> SH WHEN NOTICE DATE IS PAST
      * SH -> RC WHEN NOTICE DATE IS PAST
      * RC -> VR WHEN NOTICE DATE IS PAST
      * VR -> AD WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STRAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4040-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CCOM-STRAP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE SHIPMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOT-SHIPMENT-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-TRAN-CD
                   ADD CCOM-SHIPMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-SHIPMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4070-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOT-SHIPMENT-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 7.00 TO CCOM-COIN-YTD
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 3.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOT-SHIPMENT-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 10.00
               MOVE 10.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 35.00
               MOVE 35.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E011 CUST ID REQUIRED
      * E072 FEE AMT NEGATIVE
      * E914 FEE AMT OVER 25000
      * E663 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN CCOM-FEE-AMT < 0
                   MOVE 'E072' TO WS-REJECT-CODE
               WHEN CCOM-FEE-AMT > 25000.00
                   MOVE 'E914' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E663' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-CNCAL.
           INITIALIZE CNCAL-PARMS
           MOVE CCOM-STRAP-YTD
             TO CNCAL-STRAP-YTD
           MOVE CCOM-VARIANCE-AMT
             TO CNCAL-VARIANCE-AMT
           MOVE CCOM-FEE-YTD
             TO CNCAL-FEE-YTD
           MOVE CCOM-SHIPMENT-AMT
             TO CNCAL-SHIPMENT-AMT
           MOVE CCOM-SHIPMENT-YTD
             TO CNCAL-SHIPMENT-YTD
           MOVE CCOM-COIN-AMT
             TO CNCAL-COIN-AMT
           MOVE CCOM-EXPIRY-DATE
             TO CNCAL-EXPIRY-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNCAL-MATURITY-DATE
           MOVE CCOM-OPEN-DATE
             TO CNCAL-OPEN-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNCAL-NEXT-REVIEW-DATE
           MOVE CCOM-STATUS-CD
             TO CNCAL-STATUS-CD
           MOVE CCOM-CARRIER-CD
             TO CNCAL-CARRIER-CD
           MOVE CCOM-SOURCE-CD
             TO CNCAL-SOURCE-CD
           MOVE CCOM-CHANNEL-CD
             TO CNCAL-CHANNEL-CD
           MOVE CCOM-OVERRIDE-FLAG
             TO CNCAL-OVERRIDE-FLAG
           MOVE CCOM-EXEMPT-FLAG
             TO CNCAL-EXEMPT-FLAG
           MOVE CCOM-NOTICE-SENT-FLAG
             TO CNCAL-NOTICE-SENT-FLAG
           MOVE CCOM-NOTICE-CNT
             TO CNCAL-NOTICE-CNT
           MOVE CCOM-RETRY-CNT
             TO CNCAL-RETRY-CNT
           MOVE CCOM-FEE-PCT
             TO CNCAL-FEE-PCT
           MOVE CCOM-RATE
             TO CNCAL-RATE
           MOVE CCOM-NAME
             TO CNCAL-NAME
           MOVE CCOM-DESC
             TO CNCAL-DESC
           CALL 'CNCAL' USING CNCAL-PARMS
           EVALUATE CNCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNCAL-VARIANCE-AMT
                     TO CCOM-VARIANCE-AMT
                   MOVE CNCAL-FEE-YTD
                     TO CCOM-FEE-YTD
                   MOVE CNCAL-OVERRIDE-FLAG
                     TO CCOM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CNCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE CCOT-KEY TO RD-KEY
           MOVE CCOT-TRAN-CD TO RD-CODE
           MOVE CCOT-COIN-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE CCOT-KEY TO RD-KEY
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
           DISPLAY 'CNOEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CNOEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
