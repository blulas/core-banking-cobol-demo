      *================================================================*
      * PROGRAM  : FNNMTHE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : FOREIGN BANKNOTES - MONTH-END. READS THE FXN        *
      *            TRANSACTIONS, UPDATES THE FXN MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FOREIGN CURRENCY BANKNOTE       *
      *            ORDERS, BUYBACK AND MARGIN                          *
      * INPUT    : FXNMST (KSDS), FXNTRN, BANKCTL                      *
      * OUTPUT   : FXNOUT, FNNMTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, FNVAL, FNWAIV           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-05-06 SKN  ORIGINAL                                       *
      * 1996-06-12 BHS  NEW FOREIGN REPORT COLUMNS INC7462             *
      * 1997-03-19 BHS  PERFORMANCE - SHIPPING READ AHEAD CHG5220      *
      * 1998-02-21 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-12 MKL  MARGIN FEE SCHEDULE UPDATE REQ6229             *
      * 2000-09-05 CRP  USD FEE SCHEDULE UPDATE CHG9022                *
      * 2003-01-12 DPS  NEW FOREIGN REPORT COLUMNS REQ4557             *
      * 2004-12-27 RJT  AUDIT FINDING - MARGIN CONTROL TOTALS INC4211  *
      * 2005-06-10 RJT  SHIPPING LIMIT REVISED INC4704                 *
      * 2005-10-27 JLM  FOREIGN TABLE EXPANDED INC6092                 *
      * 2005-05-09 GAF  PERFORMANCE - FOREIGN READ AHEAD REQ3530       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNNMTHE.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "FXNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FXNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "FXNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "FXNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "FNNMTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY FXNMST.
       FD  TRAN-FILE.
       COPY FXNTRN.
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
      * WS-USD-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-USD-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000321000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000689000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000023500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000212500'.
       01  WS-USD-BAND-TBL REDEFINES WS-USD-BAND-VALUES.
           05  WS-USD-BAND-ROW OCCURS 4 TIMES.
               10  WS-USD-BAND-LOW         PIC 9(09)V99.
               10  WS-USD-BAND-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FOREIGN-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FOREIGN-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000394500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000426500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000656500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000377000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000318000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020650000000730000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023150000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028150000000108000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028200000000664000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028450000000342000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029450000000089000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039450000000389500'.
       01  WS-FOREIGN-GRID-TBL REDEFINES WS-FOREIGN-GRID-VALUES.
           05  WS-FOREIGN-GRID-ROW OCCURS 12 TIMES.
               10  WS-FOREIGN-GRID-LOW     PIC 9(09)V99.
               10  WS-FOREIGN-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHIPPING-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPPING-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000349500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000787000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000036500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000064000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000886000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018000000000312000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000243000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023100000000172500'.
       01  WS-SHIPPING-SCHED-TBL REDEFINES WS-SHIPPING-SCHED-VALUES.
           05  WS-SHIPPING-SCHED-ROW OCCURS 8 TIMES.
               10  WS-SHIPPING-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHIPPING-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-USD-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-USD-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000549500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000595000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000294000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005200000000143500'.
       01  WS-USD-TIER-TBL REDEFINES WS-USD-TIER-VALUES.
           05  WS-USD-TIER-ROW OCCURS 4 TIMES.
               10  WS-USD-TIER-LOW         PIC 9(09)V99.
               10  WS-USD-TIER-RATE        PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  FNNMTHE  FOREIGN BANKNOTES'.
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
       COPY FNVALP.
       COPY FNWAIVP.

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
               MOVE 'FXNMST' TO ABND-FILE-NAME
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
           MOVE FXNT-KEY TO FXNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-AGE-EFF
               PERFORM 3010-PRORATE-BUYBACK
               PERFORM 3020-PRORATE-USD
               PERFORM 3030-ASSESS-REVERSAL
               PERFORM 3040-SET-TYPE
               PERFORM 3050-SPLIT-USD
               PERFORM 3060-EDIT-NAME
               PERFORM 3070-SELECT-MARGIN
               PERFORM 3080-ACCRUE-USD
               PERFORM 3090-LIMIT-ITEM
               PERFORM 3100-PRORATE-ITEM
               PERFORM 3110-SPLIT-FOREIGN
               PERFORM 3120-FLAG-SHIPPING
               PERFORM 3130-LIMIT-FOREIGN
               PERFORM 3140-PRORATE-USD
               PERFORM 3150-SELECT-MARGIN
               PERFORM 3160-PRORATE-BUYBACK
               PERFORM 3170-ACCUM-STATUS
               PERFORM 3180-ASSESS-NOTICE-SENT
               PERFORM 3190-PRORATE-MARGIN
               PERFORM 3200-AGE-OPEN
               PERFORM 3210-ASSESS-OVERRIDE
               PERFORM 3220-DESCRIBE-SOURCE
               PERFORM 3230-SET-CHANNEL
               PERFORM 3240-CHECK-MATURITY
               PERFORM 3250-DESCRIBE-TRAN
               PERFORM 3260-LIMIT-USD
               PERFORM 3270-ACCRUE-SHIPPING
               PERFORM 3280-COUNT-NOTICE
               PERFORM 3290-SELECT-MARGIN
               PERFORM 3300-ASSESS-NOTICE-SENT
               PERFORM 3310-EDIT-REJECT
               PERFORM 3320-EDIT-REJECT
               PERFORM 3330-FLAG-USD
               PERFORM 3340-COUNT-EXCEPT
               PERFORM 3350-ASSESS-PREFERRED
               PERFORM 3360-AGE-EFFECTIVE
               PERFORM 3370-LIMIT-MARGIN
               PERFORM 3380-ACCRUE-FOREIGN
               PERFORM 3390-SELECT-MARGIN
               PERFORM 3400-DESCRIBE-TYPE
               PERFORM 3410-FLAG-FOREIGN
               PERFORM 3420-SET-CHANNEL
               PERFORM 3430-SET-CURRENCY
               PERFORM 3440-SET-ORDER-TYPE
               PERFORM 3450-AGE-EFF
               PERFORM 3460-ASSESS-EXEMPT
               PERFORM 3470-SELECT-PENALTY-PCT
               PERFORM 3480-ASSESS-EXEMPT
               PERFORM 3490-LIMIT-ITEM
               PERFORM 3500-SET-TRAN
               PERFORM 3510-FLAG-FOREIGN
               PERFORM 3520-LIMIT-BUYBACK
               PERFORM 3530-AGE-LAST-ACT
               PERFORM 3540-ASSESS-REVERSAL
               PERFORM 3550-AGE-CLOSE
               PERFORM 3560-FLAG-FOREIGN
               PERFORM 3570-COUNT-SEQ
               PERFORM 3580-LIMIT-FOREIGN
               PERFORM 3590-SELECT-MARGIN
               PERFORM 3600-DESCRIBE-BRANCH
               PERFORM 3610-PRORATE-BUYBACK
               PERFORM 3620-ACCUM-CHANNEL
               PERFORM 3630-SET-TRAN
               PERFORM 3640-CHECK-POST
               PERFORM 3650-EDIT-DESC
               PERFORM 3660-ACCUM-ORDER-TYPE
               PERFORM 3670-COUNT-CYCLE
               PERFORM 3680-SET-STATUS
               PERFORM 3690-COUNT-NOTICE
               PERFORM 3700-LIMIT-MARGIN
               PERFORM 3710-ACCUM-CURRENCY
               PERFORM 3720-ACCUM-STATUS
               PERFORM 3730-ASSESS-PREFERRED
               PERFORM 3740-FLAG-USD
               PERFORM 3750-SET-STATUS
               PERFORM 3760-FLAG-USD
               PERFORM 3770-ACCRUE-USD
               PERFORM 3780-COUNT-PAYMENT
               PERFORM 3790-SET-SOURCE
               PERFORM 3800-CHECK-LAST-MAINT
               PERFORM 3810-ASSESS-AUTO-RENEW
               PERFORM 3820-ACCRUE-FOREIGN
               PERFORM 3830-EDIT-OPERATOR
               PERFORM 3840-ASSESS-NOTICE-SENT
               PERFORM 3850-SET-CURRENCY
               PERFORM 3860-LIMIT-BUYBACK
               PERFORM 3870-LIMIT-FOREIGN
               PERFORM 3880-CHECK-EFF
               PERFORM 3890-ASSESS-OVERRIDE
               PERFORM 3900-SELECT-PENALTY-PCT
               PERFORM 3910-SPLIT-MARGIN
               PERFORM 3920-AGE-LAST-PAY
               PERFORM 3930-SPLIT-SHIPPING
               PERFORM 3940-ACCUM-STATUS
               PERFORM 3950-ACCRUE-PENALTY-PCT
               PERFORM 3960-ACCUM-ORDER-TYPE
               PERFORM 3970-ASSESS-REVERSAL
               PERFORM 3980-LIMIT-MARGIN
               PERFORM 3990-PRORATE-SHIPPING
               PERFORM 4000-ACCRUE-SHIPPING
               PERFORM 4010-ACCUM-TYPE
               PERFORM 4020-ACCRUE-MARGIN
               PERFORM 4030-ACCUM-ORDER-TYPE
               PERFORM 4040-SELECT-MARGIN
               PERFORM 4050-ACCUM-TYPE
               PERFORM 4060-EDIT-NAME
               PERFORM 4070-SET-CHANNEL
               PERFORM 4080-EDIT-REJECT
               PERFORM 4090-ACCUM-STATUS
               PERFORM 4100-DESCRIBE-STATUS
               PERFORM 4110-ACCUM-CURRENCY
               PERFORM 4120-ASSESS-NOTICE-SENT
               PERFORM 7030-CALL-FNVAL
               PERFORM 7020-CALL-FNWAIV
               REWRITE FXN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'FXNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM FXN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUYBACK YTD.
      *----------------------------------------------------------------*
       3010-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-BUYBACK-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-BUYBACK-YTD
           END-IF
           ADD FXNM-BUYBACK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD YTD.
      *----------------------------------------------------------------*
       3020-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-YTD ROUNDED =
                   FXNM-SHIPPING-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-USD-YTD
           END-IF
           ADD FXNM-USD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE MARGIN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3030-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNT-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNT-MARGIN-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 3.00 TO FXNT-MARGIN-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 5.00 TO FXNT-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FXNT-MARGIN-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNT-MARGIN-AMT > 0 AND FXNT-MARGIN-AMT < 2.00
               MOVE 2.00 TO FXNT-MARGIN-AMT
           END-IF
           IF FXNT-MARGIN-AMT > 75.00
               MOVE 75.00 TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN EFFECTIVE DATE IS PAST
      * FFU -> JNR WHEN EFFECTIVE DATE IS PAST
      * JNR -> UGS WHEN EFFECTIVE DATE IS PAST
      * UGS -> XUM WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT USD AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNT-USD-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNT-USD-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E463 NAME REQUIRED
      * E305 FOREIGN AMT NEGATIVE
      * E151 FOREIGN AMT OVER 50000
      * E529 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT < 0
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT > 50000.00
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E529' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD BAND TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-USD-BAND-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FXNM-FOREIGN-AMT NOT < WS-USD-BAND-LOW(WS-TX)
                   MOVE WS-USD-BAND-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-USD.
           IF FXNM-USD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-USD-YTD * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNT-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3090-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3100-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNT-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3120-FLAG-SHIPPING.
           IF FXNM-SHIPPING-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-SHIPPING-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-FOREIGN.
           IF FXNT-FOREIGN-AMT < 50.00
               MOVE 50.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-FOREIGN-AMT > 5000.00
               MOVE 5000.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE USD TIER TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-USD-TIER-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FXNM-USD-AMT NOT < WS-USD-TIER-LOW(WS-TX)
                   MOVE WS-USD-TIER-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       3160-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-BUYBACK-AMT ROUNDED =
                   FXNM-SHIPPING-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE USD AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3180-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNT-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNT-MARGIN-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 20.00 TO FXNT-MARGIN-AMT
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 10.00 TO FXNT-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FXNT-MARGIN-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-MARGIN-AMT > 0 AND FXNT-MARGIN-AMT < 5.00
               MOVE 5.00 TO FXNT-MARGIN-AMT
           END-IF
           IF FXNT-MARGIN-AMT > 100.00
               MOVE 100.00 TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-MARGIN-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE BUYBACK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3210-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 10.00 TO FXNT-BUYBACK-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 15.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 50.00
               MOVE 50.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3220-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-USD.
           IF FXNM-USD-AMT < 25.00
               MOVE 25.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 1000.00
               MOVE 1000.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNM-MARGIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN GRID TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-GRID-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-MARGIN-AMT NOT < WS-FOREIGN-GRID-LOW(WS-TX)
                   MOVE WS-FOREIGN-GRID-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE USD YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3300-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-USD-YTD *
                       FXNM-RATE / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-USD-YTD *
                       FXNM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E437 REJECT CD REQUIRED
      * E878 FOREIGN AMT NEGATIVE
      * E959 FOREIGN AMT OVER 10000
      * E507 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT < 0
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT > 10000.00
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E262 REJECT CD REQUIRED
      * E945 SHIPPING YTD NEGATIVE
      * E539 SHIPPING YTD OVER 250000
      * E853 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD < 0
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD > 250000.00
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3330-FLAG-USD.
           IF FXNM-USD-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-USD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE FOREIGN YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3350-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNM-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   COMPUTE FXNM-MARGIN-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 12.00 TO FXNM-MARGIN-AMT
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 10.00 TO FXNM-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FXNM-MARGIN-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-MARGIN-AMT > 0 AND FXNM-MARGIN-AMT < 5.00
               MOVE 5.00 TO FXNM-MARGIN-AMT
           END-IF
           IF FXNM-MARGIN-AMT > 50.00
               MOVE 50.00 TO FXNM-MARGIN-AMT
           END-IF
           ADD FXNM-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3370-LIMIT-MARGIN.
           IF FXNM-MARGIN-AMT < 100.00
               MOVE 100.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-AMT > 500.00
               MOVE 500.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-SHIPPING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN GRID TIER FOR THE SHIPPING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-GRID-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-SHIPPING-YTD NOT < WS-FOREIGN-GRID-LOW(WS-TX)
                   MOVE WS-FOREIGN-GRID-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3410-FLAG-FOREIGN.
           IF FXNT-FOREIGN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNT-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN NOTICE DATE IS PAST
      * E -> G WHEN NOTICE DATE IS PAST
      * G -> P WHEN NOTICE DATE IS PAST
      * P -> W WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN LAST PAY DATE IS PAST
      * KZV -> PHV WHEN LAST PAY DATE IS PAST
      * PHV -> PZZ WHEN LAST PAY DATE IS PAST
      * PZZ -> WMC WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MARGIN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3460-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-GRID-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-FOREIGN-GRID-LOW(WS-TX)
                   MOVE WS-FOREIGN-GRID-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE USD AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3480-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO FXNM-FOREIGN-AMT
               WHEN FXNT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO FXNM-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-FOREIGN-AMT > 0 AND FXNM-FOREIGN-AMT < 1.00
               MOVE 1.00 TO FXNM-FOREIGN-AMT
           END-IF
           IF FXNM-FOREIGN-AMT > 250.00
               MOVE 250.00 TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3490-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN EFF DATE IS PAST
      * BB -> SH WHEN EFF DATE IS PAST
      * SH -> CN WHEN EFF DATE IS PAST
      * CN -> OR WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-FOREIGN.
           IF FXNT-FOREIGN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNT-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 25.00
               MOVE 25.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 5000.00
               MOVE 5000.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE BUYBACK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3540-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNM-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   COMPUTE FXNM-USD-AMT ROUNDED = FXNT-BUYBACK-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 20.00 TO FXNM-USD-AMT
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 12.00 TO FXNM-USD-AMT
               WHEN OTHER
                   COMPUTE FXNM-USD-AMT ROUNDED = FXNT-BUYBACK-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-USD-AMT > 0 AND FXNM-USD-AMT < 1.00
               MOVE 1.00 TO FXNM-USD-AMT
           END-IF
           IF FXNM-USD-AMT > 50.00
               MOVE 50.00 TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3560-FLAG-FOREIGN.
           IF FXNM-FOREIGN-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-FOREIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-SEQ.
           IF FXNT-SEQ < 9999999
               ADD 1 TO FXNT-SEQ
           END-IF
           IF FXNT-SEQ NOT < 6
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-YTD < 100.00
               MOVE 100.00 TO FXNM-FOREIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-YTD > 50000.00
               MOVE 50000.00 TO FXNM-FOREIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD TIER TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-USD-TIER-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FXNM-FOREIGN-AMT NOT < WS-USD-TIER-LOW(WS-TX)
                   MOVE WS-USD-TIER-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN CLOSE DATE IS PAST
      * BB -> SH WHEN CLOSE DATE IS PAST
      * SH -> CN WHEN CLOSE DATE IS PAST
      * CN -> OR WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3640-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E210 DESC REQUIRED
      * E342 WORK AMT NEGATIVE
      * E123 WORK AMT OVER 10000
      * E043 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E342' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E123' TO WS-REJECT-CODE
               WHEN FXNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN OPEN DATE IS PAST
      * ESS -> XDE WHEN OPEN DATE IS PAST
      * XDE -> XFD WHEN OPEN DATE IS PAST
      * XFD -> CXH WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-MARGIN.
           IF FXNM-MARGIN-YTD < 0.00
               MOVE 0.00 TO FXNM-MARGIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-YTD > 1000.00
               MOVE 1000.00 TO FXNM-MARGIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNT-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD FXNM-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE SHIPPING YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3730-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNM-SHIPPING-YTD
                       * FXNM-RATE / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 5.00 TO FXNM-FOREIGN-AMT
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 10.00 TO FXNM-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNM-SHIPPING-YTD
                       * FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-FOREIGN-AMT > 0 AND FXNM-FOREIGN-AMT < 5.00
               MOVE 5.00 TO FXNM-FOREIGN-AMT
           END-IF
           IF FXNM-FOREIGN-AMT > 50.00
               MOVE 50.00 TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * USD YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3740-FLAG-USD.
           IF FXNM-USD-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-USD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN EFFECTIVE DATE IS PAST
      * ESS -> XDE WHEN EFFECTIVE DATE IS PAST
      * XDE -> XFD WHEN EFFECTIVE DATE IS PAST
      * XFD -> CXH WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3760-FLAG-USD.
           IF FXNM-USD-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-USD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-USD.
           IF FXNM-USD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-USD-YTD * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO FXNM-SHIPPING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN OPEN DATE IS PAST
      * FS -> HL WHEN OPEN DATE IS PAST
      * HL -> HS WHEN OPEN DATE IS PAST
      * HS -> NW WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE MARGIN YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3810-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNT-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNT-MARGIN-AMT ROUNDED = FXNM-MARGIN-YTD *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 10.00 TO FXNT-MARGIN-AMT
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 5.00 TO FXNT-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FXNT-MARGIN-AMT ROUNDED = FXNM-MARGIN-YTD *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNT-MARGIN-AMT > 0 AND FXNT-MARGIN-AMT < 2.00
               MOVE 2.00 TO FXNT-MARGIN-AMT
           END-IF
           IF FXNT-MARGIN-AMT > 35.00
               MOVE 35.00 TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNT-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E759 OPERATOR REQUIRED
      * E304 BUYBACK YTD NEGATIVE
      * E019 BUYBACK YTD OVER 99999
      * E003 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD < 0
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD > 99999.00
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3840-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = WS-WORK-AMT-2 *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO FXNM-BUYBACK-AMT
               WHEN FXNT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO FXNM-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = WS-WORK-AMT-2 *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-BUYBACK-AMT > 0 AND FXNM-BUYBACK-AMT < 10.00
               MOVE 10.00 TO FXNM-BUYBACK-AMT
           END-IF
           IF FXNM-BUYBACK-AMT > 100.00
               MOVE 100.00 TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN EFFECTIVE DATE IS PAST
      * E -> G WHEN EFFECTIVE DATE IS PAST
      * G -> P WHEN EFFECTIVE DATE IS PAST
      * P -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 10.00
               MOVE 10.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 2500.00
               MOVE 2500.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-AMT < 50.00
               MOVE 50.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-AMT > 5000.00
               MOVE 5000.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE FOREIGN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3890-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNT-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 10.00 TO FXNT-FOREIGN-AMT
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 5.00 TO FXNT-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNT-FOREIGN-AMT > 0 AND FXNT-FOREIGN-AMT < 2.00
               MOVE 2.00 TO FXNT-FOREIGN-AMT
           END-IF
           IF FXNT-FOREIGN-AMT > 35.00
               MOVE 35.00 TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE USD TIER TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3900-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-USD-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF FXNT-FOREIGN-AMT NOT < WS-USD-TIER-LOW(WS-TX)
                   MOVE WS-USD-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-YTD - (WS-WORK-AMT * 3)
           COMPUTE FXNT-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FXNM-SHIPPING-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-SHIPPING-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNM-USD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNM-BUYBACK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3960-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE USD AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3970-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = FXNT-USD-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = FXNT-USD-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-MARGIN.
           IF FXNT-MARGIN-AMT < 100.00
               MOVE 100.00 TO FXNT-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FXNT-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPPING AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-TYPE-CD
                   ADD FXNM-SHIPPING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-SHIPPING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-MARGIN.
           IF FXNM-MARGIN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-MARGIN-YTD * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNT-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY ORDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN GRID TIER FOR THE MARGIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-GRID-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-MARGIN-YTD NOT < WS-FOREIGN-GRID-LOW(WS-TX)
                   MOVE WS-FOREIGN-GRID-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-TYPE-CD
                   ADD FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E274 NAME REQUIRED
      * E242 BUYBACK AMT NEGATIVE
      * E900 BUYBACK AMT OVER 1000000
      * E868 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT < 0
                   MOVE 'E242' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT > 1000000.00
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN FXNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E868' TO WS-REJECT-CODE
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
       4070-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E088 REJECT CD REQUIRED
      * E494 MARGIN AMT NEGATIVE
      * E666 MARGIN AMT OVER 1000000
      * E246 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E088' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT < 0
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT > 1000000.00
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E246' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE MARGIN AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4120-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNT-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 7.00 TO FXNT-USD-AMT
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 5.00 TO FXNT-USD-AMT
               WHEN OTHER
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNT-USD-AMT > 0 AND FXNT-USD-AMT < 1.00
               MOVE 1.00 TO FXNT-USD-AMT
           END-IF
           IF FXNT-USD-AMT > 75.00
               MOVE 75.00 TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE FXN FIELDS TO FNVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-FNVAL.
           INITIALIZE FNVAL-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNVAL-FOREIGN-AMT
           MOVE FXNM-SHIPPING-AMT
             TO FNVAL-SHIPPING-AMT
           MOVE FXNM-FOREIGN-YTD
             TO FNVAL-FOREIGN-YTD
           MOVE FXNM-SHIPPING-YTD
             TO FNVAL-SHIPPING-YTD
           MOVE FXNM-MARGIN-AMT
             TO FNVAL-MARGIN-AMT
           MOVE FXNM-USD-AMT
             TO FNVAL-USD-AMT
           MOVE FXNM-OPEN-DATE
             TO FNVAL-OPEN-DATE
           MOVE FXNM-EXPIRY-DATE
             TO FNVAL-EXPIRY-DATE
           MOVE FXNM-NOTICE-DATE
             TO FNVAL-NOTICE-DATE
           MOVE FXNM-LAST-ACT-DATE
             TO FNVAL-LAST-ACT-DATE
           MOVE FXNM-TYPE-CD
             TO FNVAL-TYPE-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNVAL-ORDER-TYPE-CD
           MOVE FXNM-SOURCE-CD
             TO FNVAL-SOURCE-CD
           MOVE FXNM-BRANCH-CD
             TO FNVAL-BRANCH-CD
           MOVE FXNM-EXEMPT-FLAG
             TO FNVAL-EXEMPT-FLAG
           MOVE FXNM-NOTICE-SENT-FLAG
             TO FNVAL-NOTICE-SENT-FLAG
           MOVE FXNM-REVIEW-FLAG
             TO FNVAL-REVIEW-FLAG
           MOVE FXNM-EXCEPT-CNT
             TO FNVAL-EXCEPT-CNT
           MOVE FXNM-PAYMENT-CNT
             TO FNVAL-PAYMENT-CNT
           MOVE FXNM-PENALTY-PCT
             TO FNVAL-PENALTY-PCT
           MOVE FXNM-RATE
             TO FNVAL-RATE
           MOVE FXNM-CONTACT
             TO FNVAL-CONTACT
           MOVE FXNM-REF-NO
             TO FNVAL-REF-NO
           CALL 'FNVAL' USING FNVAL-PARMS
           EVALUATE FNVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNVAL-FOREIGN-AMT
                     TO FXNM-FOREIGN-AMT
                   MOVE FNVAL-SHIPPING-YTD
                     TO FXNM-SHIPPING-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-FNVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE FXN FIELDS TO FNWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-FNWAIV.
           INITIALIZE FNWAIV-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNWAIV-FOREIGN-AMT
           MOVE FXNM-BUYBACK-YTD
             TO FNWAIV-BUYBACK-YTD
           MOVE FXNM-MARGIN-YTD
             TO FNWAIV-MARGIN-YTD
           MOVE FXNM-USD-YTD
             TO FNWAIV-USD-YTD
           MOVE FXNM-MARGIN-AMT
             TO FNWAIV-MARGIN-AMT
           MOVE FXNM-USD-AMT
             TO FNWAIV-USD-AMT
           MOVE FXNM-NOTICE-DATE
             TO FNWAIV-NOTICE-DATE
           MOVE FXNM-LAST-MAINT-DATE
             TO FNWAIV-LAST-MAINT-DATE
           MOVE FXNM-EFFECTIVE-DATE
             TO FNWAIV-EFFECTIVE-DATE
           MOVE FXNM-LAST-ACT-DATE
             TO FNWAIV-LAST-ACT-DATE
           MOVE FXNM-CURRENCY-CD
             TO FNWAIV-CURRENCY-CD
           MOVE FXNM-SOURCE-CD
             TO FNWAIV-SOURCE-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNWAIV-ORDER-TYPE-CD
           MOVE FXNM-TYPE-CD
             TO FNWAIV-TYPE-CD
           MOVE FXNM-REVIEW-FLAG
             TO FNWAIV-REVIEW-FLAG
           MOVE FXNM-EXEMPT-FLAG
             TO FNWAIV-EXEMPT-FLAG
           MOVE FXNM-PREFERRED-FLAG
             TO FNWAIV-PREFERRED-FLAG
           MOVE FXNM-EXCEPT-CNT
             TO FNWAIV-EXCEPT-CNT
           MOVE FXNM-NOTICE-CNT
             TO FNWAIV-NOTICE-CNT
           MOVE FXNM-PENALTY-PCT
             TO FNWAIV-PENALTY-PCT
           MOVE FXNM-RATE
             TO FNWAIV-RATE
           MOVE FXNM-NAME
             TO FNWAIV-NAME
           MOVE FXNM-CUST-ID
             TO FNWAIV-CUST-ID
           CALL 'FNWAIV' USING FNWAIV-PARMS
           EVALUATE FNWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNWAIV-REVIEW-FLAG
                     TO FXNM-REVIEW-FLAG
                   MOVE FNWAIV-EXEMPT-FLAG
                     TO FXNM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-FNWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE FXNT-KEY TO RD-KEY
           MOVE FXNT-TRAN-CD TO RD-CODE
           MOVE FXNT-MARGIN-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE FXNT-KEY TO RD-KEY
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
           DISPLAY 'FNNMTHE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'FNNMTHE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
