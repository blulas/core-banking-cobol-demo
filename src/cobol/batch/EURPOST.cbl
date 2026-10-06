      *================================================================*
      * PROGRAM  : EURPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EURO CONVERSION - DAILY POSTING. READS THE EUR      *
      *            TRANSACTIONS, UPDATES THE EUR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. NATIONAL CURRENCY BALANCE       *
      *            CONVERSION TO EURO AND DUAL DISPLAY                 *
      * INPUT    : EURMST (KSDS), EURTRN, BANKCTL                      *
      * OUTPUT   : EUROUT, EURPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, EUVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-03-09 GAF  ORIGINAL                                       *
      * 1999-01-05 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-03-16 JLM  PERFORMANCE - CONVERTED READ AHEAD CHG9118     *
      * 2004-02-20 BHS  CONVERTED LIMIT REVISED CHG9937                *
      * 2004-12-02 BHS  NATIONAL RERUN PROTECTION REQ6095              *
      * 2006-03-21 TWH  NEW TRIANGULAR REPORT COLUMNS PRB8946          *
      * 2008-03-13 PQR  NEW ROUNDING REPORT COLUMNS CHG8041            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EURPOST.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "EURMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EURM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "EURTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "EUROUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "EURPORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY EURMST.
       FD  TRAN-FILE.
       COPY EURTRN.
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
      * WS-TRIANGUL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRIANGUL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000694000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000197500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000025000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000077000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010800000000606500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000408000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021300000000747500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023800000000388500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033800000000840500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034800000000774500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034850000000494000'.
       01  WS-TRIANGUL-TIER-TBL REDEFINES WS-TRIANGUL-TIER-VALUES.
           05  WS-TRIANGUL-TIER-ROW OCCURS 11 TIMES.
               10  WS-TRIANGUL-TIER-LOW    PIC 9(09)V99.
               10  WS-TRIANGUL-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONVERTE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONVERTE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000201000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000509500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000350000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000449000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000562500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000147000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000159000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000294500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000706000'.
       01  WS-CONVERTE-BAND-TBL REDEFINES WS-CONVERTE-BAND-VALUES.
           05  WS-CONVERTE-BAND-ROW OCCURS 9 TIMES.
               10  WS-CONVERTE-BAND-LOW    PIC 9(09)V99.
               10  WS-CONVERTE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESIDUAL-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESIDUAL-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000014500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000587500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000904000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000226000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000546500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000014500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008750000000879500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013750000000519000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014250000000012500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019250000000264000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019300000000788000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021800000000133500'.
       01  WS-RESIDUAL-GRID-TBL REDEFINES WS-RESIDUAL-GRID-VALUES.
           05  WS-RESIDUAL-GRID-ROW OCCURS 12 TIMES.
               10  WS-RESIDUAL-GRID-LOW    PIC 9(09)V99.
               10  WS-RESIDUAL-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ROUNDING-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ROUNDING-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000755000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000640000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000088500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000337000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000421000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000629000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019050000000282500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019100000000570500'.
       01  WS-ROUNDING-TIER-TBL REDEFINES WS-ROUNDING-TIER-VALUES.
           05  WS-ROUNDING-TIER-ROW OCCURS 8 TIMES.
               10  WS-ROUNDING-TIER-LOW    PIC 9(09)V99.
               10  WS-ROUNDING-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  EURPOST  EURO CONVERSION'.
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
       COPY EUVALP.

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
               MOVE 'EURMST' TO ABND-FILE-NAME
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
           MOVE EURT-KEY TO EURM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-ITEM
               PERFORM 3010-ACCUM-CHANNEL
               PERFORM 3020-AGE-LAST-MAINT
               PERFORM 3030-FLAG-TRIANGULAR
               PERFORM 3040-DESCRIBE-CURRENCY
               PERFORM 3050-PRORATE-NATIONAL
               PERFORM 3060-COUNT-CYCLE
               PERFORM 3070-ACCRUE-CONVERTED
               PERFORM 3080-COUNT-EXCEPT
               PERFORM 3090-DESCRIBE-TYPE
               PERFORM 3100-EDIT-REF-NO
               PERFORM 3110-SET-TYPE
               PERFORM 3120-CHECK-NEXT-DUE
               PERFORM 3130-SET-CURRENCY
               PERFORM 3140-DESCRIBE-REGION
               PERFORM 3150-DESCRIBE-CHANNEL
               PERFORM 3160-PRORATE-ROUNDING
               PERFORM 3170-SPLIT-EURO
               PERFORM 3180-ASSESS-REVIEW
               PERFORM 3190-EDIT-REF-NO
               PERFORM 3200-LIMIT-TRIANGULAR
               PERFORM 3210-CHECK-OPEN
               PERFORM 3220-SET-CURRENCY
               PERFORM 3230-SELECT-RATE
               PERFORM 3240-SPLIT-EURO
               PERFORM 3250-FLAG-EURO
               PERFORM 3260-LIMIT-TRIANGULAR
               PERFORM 3270-LIMIT-NATIONAL
               PERFORM 3280-ASSESS-AUTO-RENEW
               PERFORM 3290-LIMIT-ITEM
               PERFORM 3300-AGE-LAST-STMT
               PERFORM 3310-AGE-NEXT-DUE
               PERFORM 3320-ACCRUE-RESIDUAL
               PERFORM 3330-FLAG-FEE
               PERFORM 3340-COUNT-PAYMENT
               PERFORM 3350-SELECT-RATE
               PERFORM 3360-SPLIT-EURO
               PERFORM 3370-AGE-LAST-STMT
               PERFORM 3380-ASSESS-AUTO-RENEW
               PERFORM 3390-SELECT-FEE-PCT
               PERFORM 3400-DESCRIBE-CHANNEL
               PERFORM 3410-SPLIT-CONVERTED
               PERFORM 3420-SET-TYPE
               PERFORM 3430-EDIT-NAME
               PERFORM 3440-DESCRIBE-CHANNEL
               PERFORM 3450-CHECK-LAST-PAY
               PERFORM 3460-FLAG-TRIANGULAR
               PERFORM 3470-SET-CHANNEL
               PERFORM 3480-FLAG-RESIDUAL
               PERFORM 3490-CHECK-EFFECTIVE
               PERFORM 3500-FLAG-FEE
               PERFORM 3510-COUNT-SEQ
               PERFORM 3520-SELECT-FEE-PCT
               PERFORM 3530-FLAG-TRIANGULAR
               PERFORM 3540-EDIT-DESC
               PERFORM 3550-SELECT-FEE-PCT
               PERFORM 3560-COUNT-ITEM
               PERFORM 3570-PRORATE-ROUNDING
               PERFORM 3580-COUNT-ITEM
               PERFORM 3590-SPLIT-ROUNDING
               PERFORM 3600-SPLIT-RESIDUAL
               PERFORM 3610-ASSESS-REVERSAL
               PERFORM 3620-ACCUM-CHANNEL
               PERFORM 3630-DESCRIBE-CHANNEL
               PERFORM 3640-LIMIT-RESIDUAL
               PERFORM 3650-CHECK-LAST-STMT
               PERFORM 3660-PRORATE-RESIDUAL
               PERFORM 3670-SPLIT-ROUNDING
               PERFORM 3680-COUNT-CYCLE
               PERFORM 3690-EDIT-MEMO
               PERFORM 3700-LIMIT-FEE
               PERFORM 3710-PRORATE-NATIONAL
               PERFORM 3720-SELECT-FEE-PCT
               PERFORM 3730-DESCRIBE-CHANNEL
               PERFORM 3740-EDIT-CONTACT
               PERFORM 3750-DESCRIBE-CHANNEL
               PERFORM 3760-CHECK-MATURITY
               PERFORM 3770-PRORATE-NATIONAL
               PERFORM 3780-AGE-EFFECTIVE
               PERFORM 3790-ACCUM-TRAN
               PERFORM 3800-CHECK-OPEN
               PERFORM 3810-ACCRUE-NATIONAL
               PERFORM 3820-SPLIT-TRIANGULAR
               PERFORM 3830-DESCRIBE-CURRENCY
               PERFORM 3840-PRORATE-CONVERTED
               PERFORM 3850-ACCUM-CHANNEL
               PERFORM 3860-CHECK-POST
               PERFORM 3870-FLAG-ROUNDING
               PERFORM 3880-EDIT-CONTACT
               PERFORM 3890-DESCRIBE-TRAN
               PERFORM 3900-SPLIT-NATIONAL
               PERFORM 3910-AGE-LAST-MAINT
               PERFORM 3920-ASSESS-REVIEW
               PERFORM 3930-AGE-MATURITY
               PERFORM 3940-PRORATE-FEE
               PERFORM 3950-COUNT-ITEM
               PERFORM 3960-CHECK-OPEN
               PERFORM 3970-ACCRUE-ROUNDING
               PERFORM 3980-ASSESS-AUTO-RENEW
               PERFORM 3990-SET-CHANNEL
               PERFORM 4000-CHECK-LAST-STMT
               PERFORM 4010-FLAG-TRIANGULAR
               PERFORM 4020-EDIT-MEMO
               PERFORM 4030-SPLIT-EURO
               PERFORM 4040-EDIT-CONTACT
               PERFORM 4050-AGE-LAST-STMT
               PERFORM 4060-SET-CHANNEL
               PERFORM 4070-ACCRUE-PENALTY-PCT
               PERFORM 4080-CHECK-MATURITY
               PERFORM 4090-ACCRUE-NATIONAL
               PERFORM 4100-AGE-LAST-STMT
               PERFORM 4110-SELECT-RATE
               PERFORM 4120-COUNT-ITEM
               PERFORM 4130-AGE-LAST-STMT
               PERFORM 4140-CHECK-NEXT-REVIEW
               PERFORM 4150-SELECT-PENALTY-PCT
               PERFORM 4160-LIMIT-NATIONAL
               PERFORM 4170-AGE-LAST-STMT
               PERFORM 4180-FLAG-ITEM
               PERFORM 4190-DESCRIBE-CHANNEL
               PERFORM 4200-COUNT-ITEM
               PERFORM 4210-ASSESS-PREFERRED
               PERFORM 4220-FLAG-TRIANGULAR
               PERFORM 4230-CHECK-LAST-PAY
               PERFORM 4240-FLAG-ROUNDING
               PERFORM 4250-SELECT-RATE
               PERFORM 4260-FLAG-CONVERTED
               PERFORM 4270-LIMIT-FEE
               PERFORM 4280-DESCRIBE-CURRENCY
               PERFORM 4290-EDIT-OPERATOR
               PERFORM 4300-EDIT-DESC
               PERFORM 4310-ASSESS-AUTO-RENEW
               PERFORM 4320-ASSESS-REVERSAL
               PERFORM 4330-FLAG-EURO
               PERFORM 4340-SELECT-RATE
               PERFORM 4350-AGE-NEXT-REVIEW
               PERFORM 4360-LIMIT-ITEM
               PERFORM 4370-PRORATE-RESIDUAL
               PERFORM 4380-SELECT-RATE
               PERFORM 4390-ACCRUE-FEE
               PERFORM 4400-SET-TRAN
               PERFORM 4410-AGE-NEXT-DUE
               PERFORM 7040-CALL-EUVAL
               REWRITE EUR-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'EURMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM EUR-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3030-FLAG-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-TRIANGULAR-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE NATIONAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3050-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   EURT-NATIONAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-CONVERTED.
           IF EURM-CONVERTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-CONVERTED-YTD * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-EURO-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-EXCEPT.
           IF EURM-EXCEPT-CNT < 9999999
               ADD 1 TO EURM-EXCEPT-CNT
           END-IF
           IF EURM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E171 REF NO REQUIRED
      * E233 EURO YTD NEGATIVE
      * E442 EURO YTD OVER 250000
      * E593 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-REF-NO = SPACES
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD < 0
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD > 250000.00
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN EURM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN NEXT DUE DATE IS PAST
      * SMF -> UEC WHEN NEXT DUE DATE IS PAST
      * UEC -> VJA WHEN NEXT DUE DATE IS PAST
      * VJA -> WPH WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF EURM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EURT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * EW -> FJ WHEN EFFECTIVE DATE IS PAST
      * FJ -> JC WHEN EFFECTIVE DATE IS PAST
      * JC -> EW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EW' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-ROUNDING-YTD ROUNDED =
                   EURM-TRIANGULAR-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-ROUNDING-YTD
           END-IF
           ADD EURM-ROUNDING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT EURO AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-AMT - (WS-WORK-AMT * 3)
           COMPUTE EURM-ROUNDING-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE RESIDUAL YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3180-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EURM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   COMPUTE EURM-FEE-YTD ROUNDED = EURM-RESIDUAL-YTD *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-STATUS-CD = 'B'
                   MOVE 7.00 TO EURM-FEE-YTD
               WHEN EURM-STATUS-CD = 'F'
                   MOVE 7.00 TO EURM-FEE-YTD
               WHEN OTHER
                   COMPUTE EURM-FEE-YTD ROUNDED = EURM-RESIDUAL-YTD *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-FEE-YTD > 0 AND EURM-FEE-YTD < 2.00
               MOVE 2.00 TO EURM-FEE-YTD
           END-IF
           IF EURM-FEE-YTD > 75.00
               MOVE 75.00 TO EURM-FEE-YTD
           END-IF
           ADD EURM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E603 REF NO REQUIRED
      * E149 TRIANGULAR AMT NEGATIVE
      * E285 TRIANGULAR AMT OVER 5000
      * E889 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-REF-NO = SPACES
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT < 0
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT > 5000.00
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN EURM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD < 100.00
               MOVE 100.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-YTD > 1000.00
               MOVE 1000.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF EURM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO EURM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * EW -> FJ WHEN EFF DATE IS PAST
      * FJ -> JC WHEN EFF DATE IS PAST
      * JC -> EW WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EW' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE EURO YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-EURO-YTD NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-AMT - (WS-WORK-AMT * 12)
           COMPUTE EURM-RESIDUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3250-FLAG-EURO.
           IF EURM-EURO-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-EURO-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT < 50.00
               MOVE 50.00 TO EURM-TRIANGULAR-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-AMT > 10000.00
               MOVE 10000.00 TO EURM-TRIANGULAR-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3270-LIMIT-NATIONAL.
           IF EURT-NATIONAL-AMT < 100.00
               MOVE 100.00 TO EURT-NATIONAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-NATIONAL-AMT > 2500.00
               MOVE 2500.00 TO EURT-NATIONAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3280-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = WS-WORK-AMT-2 *
                       EURM-PENALTY-PCT / 200
               WHEN EURT-TRAN-CD = 'CV'
                   MOVE 20.00 TO EURM-TRIANGULAR-YTD
               WHEN EURT-TRAN-CD = 'RV'
                   MOVE 20.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = WS-WORK-AMT-2 *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 1.00
               MOVE 1.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 250.00
               MOVE 250.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3290-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESIDUAL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-RESIDUAL-YTD * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURT-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3330-FLAG-FEE.
           IF EURT-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE ROUNDING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-ROUNDING-YTD NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-YTD - (WS-WORK-AMT * 3)
           COMPUTE EURM-EURO-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED YTD FEE ON THE EURO YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3380-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-CONVERTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-REVIEW-FLAG = 'Y'
                   COMPUTE EURM-CONVERTED-YTD ROUNDED = EURM-EURO-YTD *
                       EURM-FEE-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 7.00 TO EURM-CONVERTED-YTD
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 10.00 TO EURM-CONVERTED-YTD
               WHEN OTHER
                   COMPUTE EURM-CONVERTED-YTD ROUNDED = EURM-EURO-YTD *
                       EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-CONVERTED-YTD > 0 AND EURM-CONVERTED-YTD < 2.00
               MOVE 2.00 TO EURM-CONVERTED-YTD
           END-IF
           IF EURM-CONVERTED-YTD > 150.00
               MOVE 150.00 TO EURM-CONVERTED-YTD
           END-IF
           ADD EURM-CONVERTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE TRIANGULAR YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-TRIANGULAR-YTD NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EURM-CONVERTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-CONVERTED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EURT-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN LAST STMT DATE IS PAST
      * SMF -> UEC WHEN LAST STMT DATE IS PAST
      * UEC -> VJA WHEN LAST STMT DATE IS PAST
      * VJA -> WPH WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E330 NAME REQUIRED
      * E362 FEE AMT NEGATIVE
      * E307 FEE AMT OVER 250000
      * E266 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN EURM-FEE-AMT < 0
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN EURM-FEE-AMT > 250000.00
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN EURM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E266' TO WS-REJECT-CODE
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
       3440-DESCRIBE-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO EURM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3460-FLAG-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-TRIANGULAR-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN EFFECTIVE DATE IS PAST
      * EBM -> GAU WHEN EFFECTIVE DATE IS PAST
      * GAU -> GSU WHEN EFFECTIVE DATE IS PAST
      * GSU -> VWP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESIDUAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3480-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF EURM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3500-FLAG-FEE.
           IF EURM-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 6
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE TRIANGULAR AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-TRIANGULAR-AMT NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3530-FLAG-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E265 DESC REQUIRED
      * E338 WORK AMT NEGATIVE
      * E889 WORK AMT OVER 5000
      * E399 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-DESC = SPACES
                   MOVE 'E265' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN EURM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E399' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE ROUNDING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-ROUNDING-AMT NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 10
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-ROUNDING-AMT ROUNDED =
                   EURM-TRIANGULAR-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-ROUNDING-AMT
           END-IF
           ADD EURM-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 6
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURM-ROUNDING-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURM-ROUNDING-AMT - (WS-WORK-AMT * 3)
           COMPUTE EURT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESIDUAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-RESIDUAL.
           COMPUTE WS-WORK-AMT = EURM-RESIDUAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURM-RESIDUAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE EURM-TRIANGULAR-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE TRIANGULAR AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3610-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = EURM-TRIANGULAR-AMT *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-CONV-METHOD-CD = 'GL'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN EURM-CONV-METHOD-CD = 'JA'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = EURM-TRIANGULAR-AMT *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-CHANNEL-CD
                   ADD EURT-ROUNDING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURT-ROUNDING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESIDUAL YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-RESIDUAL.
           IF EURM-RESIDUAL-YTD < 0.00
               MOVE 0.00 TO EURM-RESIDUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-RESIDUAL-YTD > 50000.00
               MOVE 50000.00 TO EURM-RESIDUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESIDUAL AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-AMT ROUNDED =
                   EURM-EURO-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURM-ROUNDING-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURM-ROUNDING-AMT - (WS-WORK-AMT * 4)
           COMPUTE EURM-TRIANGULAR-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E494 MEMO REQUIRED
      * E381 CONVERTED AMT NEGATIVE
      * E949 CONVERTED AMT OVER 99999
      * E711 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-MEMO = SPACES
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-AMT < 0
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-AMT > 99999.00
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN EURM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-FEE.
           IF EURT-FEE-AMT < 100.00
               MOVE 100.00 TO EURT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-FEE-AMT > 10000.00
               MOVE 10000.00 TO EURT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESIDUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL YTD.
      *----------------------------------------------------------------*
       3710-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-NATIONAL-YTD ROUNDED =
                   EURM-RESIDUAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-NATIONAL-YTD
           END-IF
           ADD EURM-NATIONAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE TRIANGULAR YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-TRIANGULAR-YTD NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E598 CONTACT REQUIRED
      * E720 ROUNDING AMT NEGATIVE
      * E487 ROUNDING AMT OVER 250000
      * E727 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN EURT-ROUNDING-AMT < 0
                   MOVE 'E720' TO WS-REJECT-CODE
               WHEN EURT-ROUNDING-AMT > 250000.00
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN EURM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E727' TO WS-REJECT-CODE
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
       3750-DESCRIBE-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EURM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EURM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL YTD.
      *----------------------------------------------------------------*
       3770-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-NATIONAL-YTD ROUNDED =
                   EURM-EURO-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-NATIONAL-YTD
           END-IF
           ADD EURM-NATIONAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-TRAN-CD
                   ADD EURM-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF EURM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-NATIONAL.
           IF EURT-NATIONAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-NATIONAL-AMT * EURM-RATE / 100 / 360
               ADD WS-WORK-AMT TO EURM-EURO-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-AMT - (WS-WORK-AMT *
               4)
           COMPUTE EURT-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONVERTED AMT.
      *----------------------------------------------------------------*
       3840-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-CONVERTED-AMT ROUNDED =
                   EURT-CONVERTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-CONVERTED-AMT
           END-IF
           ADD EURM-CONVERTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-CHANNEL-CD
                   ADD EURM-CONVERTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-CONVERTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF EURT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO EURT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-ROUNDING.
           IF EURM-ROUNDING-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-ROUNDING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E181 CONTACT REQUIRED
      * E702 CONVERTED AMT NEGATIVE
      * E794 CONVERTED AMT OVER 99999
      * E521 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-AMT < 0
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-AMT > 99999.00
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN EURM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EURM-NATIONAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURM-NATIONAL-AMT - (WS-WORK-AMT * 4)
           COMPUTE EURM-RESIDUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE TRIANGULAR YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3920-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EURM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EURM-TRIANGULAR-YTD *
                       EURM-FEE-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EURM-TRIANGULAR-YTD *
                       EURM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURT-FEE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURT-FEE-AMT
           END-IF
           ADD EURT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 3
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF EURM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-ROUNDING.
           IF EURT-ROUNDING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-ROUNDING-AMT * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED FEE ON THE NATIONAL AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3980-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-CONVERTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURM-CONVERTED-AMT ROUNDED =
                       EURT-NATIONAL-AMT * EURM-RATE / 200
               WHEN EURM-TYPE-CD = 'AGL'
                   MOVE 10.00 TO EURM-CONVERTED-AMT
               WHEN EURM-TYPE-CD = 'SMF'
                   MOVE 15.00 TO EURM-CONVERTED-AMT
               WHEN OTHER
                   COMPUTE EURM-CONVERTED-AMT ROUNDED =
                       EURT-NATIONAL-AMT * EURM-RATE / 100
           END-EVALUATE
           IF EURM-CONVERTED-AMT > 0 AND EURM-CONVERTED-AMT < 1.00
               MOVE 1.00 TO EURM-CONVERTED-AMT
           END-IF
           IF EURM-CONVERTED-AMT > 25.00
               MOVE 25.00 TO EURM-CONVERTED-AMT
           END-IF
           ADD EURM-CONVERTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN LAST MAINT DATE IS PAST
      * EBM -> GAU WHEN LAST MAINT DATE IS PAST
      * GAU -> GSU WHEN LAST MAINT DATE IS PAST
      * GSU -> VWP WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EURM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4010-FLAG-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-TRIANGULAR-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E749 MEMO REQUIRED
      * E695 EURO YTD NEGATIVE
      * E678 EURO YTD OVER 99999
      * E003 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-MEMO = SPACES
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD < 0
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD > 99999.00
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN EURM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-AMT - (WS-WORK-AMT * 2)
           COMPUTE EURM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E465 CONTACT REQUIRED
      * E605 RESIDUAL YTD NEGATIVE
      * E706 RESIDUAL YTD OVER 25000
      * E700 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-YTD < 0
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-YTD > 25000.00
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN EURM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN OPEN DATE IS PAST
      * EBM -> GAU WHEN OPEN DATE IS PAST
      * GAU -> GSU WHEN OPEN DATE IS PAST
      * GSU -> VWP WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EURM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EURM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-NATIONAL.
           IF EURM-NATIONAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-NATIONAL-AMT * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURM-EURO-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4100-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE ROUNDING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURT-ROUNDING-AMT NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 10
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EURM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO EURM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROUNDING TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROUNDING-TIER-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF EURM-FEE-AMT NOT < WS-ROUNDING-TIER-LOW(WS-TX)
                   MOVE WS-ROUNDING-TIER-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4160-LIMIT-NATIONAL.
           IF EURT-NATIONAL-AMT < 25.00
               MOVE 25.00 TO EURT-NATIONAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-NATIONAL-AMT > 2500.00
               MOVE 2500.00 TO EURT-NATIONAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4180-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 5
               MOVE 'Y' TO EURM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE CONVERTED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4210-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EURM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURM-FEE-AMT ROUNDED = EURM-CONVERTED-YTD *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-CONV-METHOD-CD = 'GL'
                   MOVE 20.00 TO EURM-FEE-AMT
               WHEN EURM-CONV-METHOD-CD = 'JA'
                   MOVE 10.00 TO EURM-FEE-AMT
               WHEN OTHER
                   COMPUTE EURM-FEE-AMT ROUNDED = EURM-CONVERTED-YTD *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-FEE-AMT > 0 AND EURM-FEE-AMT < 2.00
               MOVE 2.00 TO EURM-FEE-AMT
           END-IF
           IF EURM-FEE-AMT > 75.00
               MOVE 75.00 TO EURM-FEE-AMT
           END-IF
           ADD EURM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4220-FLAG-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-TRIANGULAR-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO EURM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4240-FLAG-ROUNDING.
           IF EURT-ROUNDING-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURT-ROUNDING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESIDUAL GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESIDUAL-GRID-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-RESIDUAL-GRID-LOW(WS-TX)
                   MOVE WS-RESIDUAL-GRID-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4260-FLAG-CONVERTED.
           IF EURT-CONVERTED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURT-CONVERTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-FEE.
           IF EURT-FEE-AMT < 25.00
               MOVE 25.00 TO EURT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-FEE-AMT > 10000.00
               MOVE 10000.00 TO EURT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4280-DESCRIBE-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E096 OPERATOR REQUIRED
      * E276 FEE AMT NEGATIVE
      * E321 FEE AMT OVER 5000
      * E533 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-OPERATOR = SPACES
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN EURM-FEE-AMT < 0
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN EURM-FEE-AMT > 5000.00
                   MOVE 'E321' TO WS-REJECT-CODE
               WHEN EURM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E482 DESC REQUIRED
      * E534 TRIANGULAR YTD NEGATIVE
      * E372 TRIANGULAR YTD OVER 25000
      * E981 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-DESC = SPACES
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-YTD < 0
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-YTD > 25000.00
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN EURM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E981' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE ROUNDING AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4310-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED =
                       EURT-ROUNDING-AMT * EURM-PENALTY-PCT / 200
               WHEN EURM-STATUS-CD = 'B'
                   MOVE 5.00 TO EURM-TRIANGULAR-YTD
               WHEN EURM-STATUS-CD = 'F'
                   MOVE 7.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED =
                       EURT-ROUNDING-AMT * EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 5.00
               MOVE 5.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 100.00
               MOVE 100.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING FEE ON THE ROUNDING YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4320-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO EURM-ROUNDING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   COMPUTE EURM-ROUNDING-AMT ROUNDED = EURM-ROUNDING-YTD
                       * EURM-FEE-PCT / 200
               WHEN EURT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO EURM-ROUNDING-AMT
               WHEN EURT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO EURM-ROUNDING-AMT
               WHEN OTHER
                   COMPUTE EURM-ROUNDING-AMT ROUNDED = EURM-ROUNDING-YTD
                       * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-ROUNDING-AMT > 0 AND EURM-ROUNDING-AMT < 5.00
               MOVE 5.00 TO EURM-ROUNDING-AMT
           END-IF
           IF EURM-ROUNDING-AMT > 75.00
               MOVE 75.00 TO EURM-ROUNDING-AMT
           END-IF
           ADD EURM-ROUNDING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EURO YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4330-FLAG-EURO.
           IF EURM-EURO-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-EURO-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE ROUNDING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-ROUNDING-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4350-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EURM-NEXT-REVIEW-DATE > 0 AND EURM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURT-NATIONAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURT-NATIONAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD EURT-NATIONAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD EURT-NATIONAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-NATIONAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4360-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESIDUAL AMT.
      *----------------------------------------------------------------*
       4370-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-AMT ROUNDED =
                   EURM-TRIANGULAR-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE BAND TIER FOR THE EURO AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4380-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-BAND-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-EURO-AMT NOT < WS-CONVERTE-BAND-LOW(WS-TX)
                   MOVE WS-CONVERTE-BAND-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4390-ACCRUE-FEE.
           IF EURT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-FEE-AMT * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURT-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN MATURITY DATE IS PAST
      * RV -> AD WHEN MATURITY DATE IS PAST
      * AD -> RD WHEN MATURITY DATE IS PAST
      * RD -> FX WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE EUR FIELDS TO EUVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-EUVAL.
           INITIALIZE EUVAL-PARMS
           MOVE EURM-TRIANGULAR-YTD
             TO EUVAL-TRIANGULAR-YTD
           MOVE EURM-NATIONAL-YTD
             TO EUVAL-NATIONAL-YTD
           MOVE EURM-NATIONAL-AMT
             TO EUVAL-NATIONAL-AMT
           MOVE EURM-CONVERTED-AMT
             TO EUVAL-CONVERTED-AMT
           MOVE EURM-TRIANGULAR-AMT
             TO EUVAL-TRIANGULAR-AMT
           MOVE EURM-RESIDUAL-YTD
             TO EUVAL-RESIDUAL-YTD
           MOVE EURM-LAST-MAINT-DATE
             TO EUVAL-LAST-MAINT-DATE
           MOVE EURM-MATURITY-DATE
             TO EUVAL-MATURITY-DATE
           MOVE EURM-NEXT-REVIEW-DATE
             TO EUVAL-NEXT-REVIEW-DATE
           MOVE EURM-NOTICE-DATE
             TO EUVAL-NOTICE-DATE
           MOVE EURM-CONV-METHOD-CD
             TO EUVAL-CONV-METHOD-CD
           MOVE EURM-REGION-CD
             TO EUVAL-REGION-CD
           MOVE EURM-CURRENCY-CD
             TO EUVAL-CURRENCY-CD
           MOVE EURM-STATUS-CD
             TO EUVAL-STATUS-CD
           MOVE EURM-PREFERRED-FLAG
             TO EUVAL-PREFERRED-FLAG
           MOVE EURM-OVERRIDE-FLAG
             TO EUVAL-OVERRIDE-FLAG
           MOVE EURM-EXEMPT-FLAG
             TO EUVAL-EXEMPT-FLAG
           MOVE EURM-ITEM-CNT
             TO EUVAL-ITEM-CNT
           MOVE EURM-PAYMENT-CNT
             TO EUVAL-PAYMENT-CNT
           MOVE EURM-PENALTY-PCT
             TO EUVAL-PENALTY-PCT
           MOVE EURM-FEE-PCT
             TO EUVAL-FEE-PCT
           MOVE EURM-DESC
             TO EUVAL-DESC
           MOVE EURM-CONTACT
             TO EUVAL-CONTACT
           CALL 'EUVAL' USING EUVAL-PARMS
           EVALUATE EUVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE EUVAL-TRIANGULAR-AMT
                     TO EURM-TRIANGULAR-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-EUVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE EURT-KEY TO RD-KEY
           MOVE EURT-TRAN-CD TO RD-CODE
           MOVE EURT-CONVERTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE EURT-KEY TO RD-KEY
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
           DISPLAY 'EURPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'EURPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
