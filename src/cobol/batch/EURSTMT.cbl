      *================================================================*
      * PROGRAM  : EURSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EURO CONVERSION - STATEMENT PREPARATION. READS THE  *
      *            EUR TRANSACTIONS, UPDATES THE EUR MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. NATIONAL CURRENCY      *
      *            BALANCE CONVERSION TO EURO AND DUAL DISPLAY         *
      * INPUT    : EURMST (KSDS), EURTRN, BANKCTL                      *
      * OUTPUT   : EUROUT, EURSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, EUPRC, EUVAL, EUFEE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-09-19 LMB  ORIGINAL                                       *
      * 1997-10-12 SKN  ROUNDING CALCULATION CORRECTED REQ5548         *
      * 1998-12-21 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-11-10 EWS  CONVERTED STATUS CODES ADDED PRB2194           *
      * 2001-08-08 SKN  PERFORMANCE - EURO READ AHEAD INC7919          *
      * 2002-03-04 MKL  EURO FEE SCHEDULE UPDATE CHG4942               *
      * 2003-05-24 EWS  RESIDUAL RERUN PROTECTION INC1772              *
      * 2004-12-02 DPS  RESIDUAL ROUNDING - HALF UP REQ3758            *
      * 2007-04-08 LMB  TRIANGULAR ROUNDING - HALF UP REQ3788          *
      * 2008-02-08 SKN  TRIANGULAR FEE SCHEDULE UPDATE INC4971         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EURSTMT.
       AUTHOR. C R PATEL.

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
           SELECT REPORT-FILE ASSIGN TO "EURSTRPT"
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
                                           VALUE 25.000000.
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
      * WS-ROUNDING-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ROUNDING-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000417500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000841000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000179500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000306500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000525500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035250000000751000'.
       01  WS-ROUNDING-TIER-TBL REDEFINES WS-ROUNDING-TIER-VALUES.
           05  WS-ROUNDING-TIER-ROW OCCURS 6 TIMES.
               10  WS-ROUNDING-TIER-LOW    PIC 9(09)V99.
               10  WS-ROUNDING-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EURO-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EURO-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000120500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000600000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000657000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000814000'.
       01  WS-EURO-GRID-TBL REDEFINES WS-EURO-GRID-VALUES.
           05  WS-EURO-GRID-ROW OCCURS 4 TIMES.
               10  WS-EURO-GRID-LOW        PIC 9(09)V99.
               10  WS-EURO-GRID-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  EURSTMT  EURO CONVERSION'.
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
       COPY EUPRCP.
       COPY EUVALP.
       COPY EUFEEP.

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
               PERFORM 3000-AGE-MATURITY
               PERFORM 3010-AGE-NOTICE
               PERFORM 3020-PRORATE-ROUNDING
               PERFORM 3030-ASSESS-REVERSAL
               PERFORM 3040-EDIT-NAME
               PERFORM 3050-DESCRIBE-TRAN
               PERFORM 3060-FLAG-NATIONAL
               PERFORM 3070-EDIT-CUST-ID
               PERFORM 3080-COUNT-CYCLE
               PERFORM 3090-SET-TRAN
               PERFORM 3100-EDIT-DESC
               PERFORM 3110-ACCRUE-CONVERTED
               PERFORM 3120-SELECT-RATE
               PERFORM 3130-CHECK-NEXT-REVIEW
               PERFORM 3140-EDIT-NAME
               PERFORM 3150-ASSESS-REVERSAL
               PERFORM 3160-ASSESS-PREFERRED
               PERFORM 3170-LIMIT-RESIDUAL
               PERFORM 3180-DESCRIBE-TRAN
               PERFORM 3190-SELECT-PENALTY-PCT
               PERFORM 3200-AGE-MATURITY
               PERFORM 3210-SET-CONV-METHOD
               PERFORM 3220-SET-STATUS
               PERFORM 3230-COUNT-CYCLE
               PERFORM 3240-EDIT-REF-NO
               PERFORM 3250-CHECK-LAST-PAY
               PERFORM 3260-ASSESS-REVIEW
               PERFORM 3270-AGE-EFF
               PERFORM 3280-SELECT-FEE-PCT
               PERFORM 3290-ACCUM-TRAN
               PERFORM 3300-DESCRIBE-CONV-METHOD
               PERFORM 3310-FLAG-ROUNDING
               PERFORM 3320-DESCRIBE-STATUS
               PERFORM 3330-EDIT-ADDR
               PERFORM 3340-SET-CONV-METHOD
               PERFORM 3350-SPLIT-CONVERTED
               PERFORM 3360-AGE-LAST-PAY
               PERFORM 3370-ACCRUE-NATIONAL
               PERFORM 3380-ASSESS-REVIEW
               PERFORM 3390-ACCUM-REGION
               PERFORM 3400-SPLIT-NATIONAL
               PERFORM 3410-CHECK-LAST-MAINT
               PERFORM 3420-SELECT-FEE-PCT
               PERFORM 3430-FLAG-NATIONAL
               PERFORM 3440-AGE-LAST-PAY
               PERFORM 3450-EDIT-CONTACT
               PERFORM 3460-EDIT-REJECT
               PERFORM 3470-COUNT-PAYMENT
               PERFORM 3480-SET-TRAN
               PERFORM 3490-AGE-EFFECTIVE
               PERFORM 3500-DESCRIBE-CURRENCY
               PERFORM 3510-SELECT-FEE-PCT
               PERFORM 3520-SET-CHANNEL
               PERFORM 3530-SPLIT-CONVERTED
               PERFORM 3540-LIMIT-TRIANGULAR
               PERFORM 3550-SELECT-FEE-PCT
               PERFORM 3560-SET-STATUS
               PERFORM 3570-DESCRIBE-REGION
               PERFORM 3580-ACCUM-REGION
               PERFORM 3590-PRORATE-FEE
               PERFORM 3600-AGE-MATURITY
               PERFORM 3610-SET-TYPE
               PERFORM 3620-EDIT-NAME
               PERFORM 3630-SET-CONV-METHOD
               PERFORM 3640-LIMIT-FEE
               PERFORM 3650-COUNT-ITEM
               PERFORM 3660-EDIT-DESC
               PERFORM 3670-ASSESS-STOP
               PERFORM 3680-DESCRIBE-REGION
               PERFORM 3690-CHECK-MATURITY
               PERFORM 3700-LIMIT-FEE
               PERFORM 3710-ACCUM-CURRENCY
               PERFORM 3720-AGE-NOTICE
               PERFORM 3730-ACCRUE-NATIONAL
               PERFORM 3740-LIMIT-EURO
               PERFORM 3750-DESCRIBE-CONV-METHOD
               PERFORM 3760-ACCUM-CHANNEL
               PERFORM 3770-DESCRIBE-REGION
               PERFORM 3780-SPLIT-ROUNDING
               PERFORM 3790-SPLIT-ROUNDING
               PERFORM 3800-SPLIT-NATIONAL
               PERFORM 3810-SET-CONV-METHOD
               PERFORM 3820-AGE-POST
               PERFORM 3830-ACCUM-CURRENCY
               PERFORM 3840-SET-CHANNEL
               PERFORM 3850-SPLIT-NATIONAL
               PERFORM 3860-SPLIT-TRIANGULAR
               PERFORM 3870-FLAG-EURO
               PERFORM 3880-FLAG-NATIONAL
               PERFORM 3890-LIMIT-FEE
               PERFORM 3900-SET-CONV-METHOD
               PERFORM 3910-SELECT-FEE-PCT
               PERFORM 3920-COUNT-CYCLE
               PERFORM 3930-LIMIT-EURO
               PERFORM 3940-FLAG-FEE
               PERFORM 3950-DESCRIBE-CHANNEL
               PERFORM 3960-EDIT-REJECT
               PERFORM 3970-ACCRUE-NATIONAL
               PERFORM 3980-CHECK-OPEN
               PERFORM 3990-AGE-LAST-PAY
               PERFORM 4000-ACCRUE-EURO
               PERFORM 4010-LIMIT-ROUNDING
               PERFORM 4020-LIMIT-CONVERTED
               PERFORM 4030-DESCRIBE-CHANNEL
               PERFORM 4040-AGE-LAST-MAINT
               PERFORM 4050-ASSESS-OVERRIDE
               PERFORM 4060-LIMIT-FEE
               PERFORM 4070-CHECK-NEXT-DUE
               PERFORM 4080-EDIT-DESC
               PERFORM 4090-PRORATE-RESIDUAL
               PERFORM 4100-SELECT-PENALTY-PCT
               PERFORM 4110-COUNT-EXCEPT
               PERFORM 4120-AGE-NEXT-REVIEW
               PERFORM 4130-CHECK-LAST-MAINT
               PERFORM 4140-AGE-OPEN
               PERFORM 4150-SPLIT-TRIANGULAR
               PERFORM 4160-ACCUM-CONV-METHOD
               PERFORM 4170-ASSESS-STOP
               PERFORM 4180-ACCRUE-FEE
               PERFORM 4190-PRORATE-EURO
               PERFORM 4200-SET-CHANNEL
               PERFORM 4210-SPLIT-TRIANGULAR
               PERFORM 4220-CHECK-NOTICE
               PERFORM 4230-FLAG-NATIONAL
               PERFORM 4240-DESCRIBE-REGION
               PERFORM 4250-ASSESS-OVERRIDE
               PERFORM 4260-AGE-EFFECTIVE
               PERFORM 4270-SPLIT-TRIANGULAR
               PERFORM 4280-SPLIT-TRIANGULAR
               PERFORM 7010-CALL-EUPRC
               PERFORM 7040-CALL-EUVAL
               PERFORM 7020-CALL-EUFEE
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
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-MATURITY.
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
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       3020-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-ROUNDING-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-ROUNDING-AMT
           END-IF
           ADD EURM-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3030-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO EURM-ROUNDING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   COMPUTE EURM-ROUNDING-AMT ROUNDED = EURM-FEE-AMT *
                       EURM-FEE-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 10.00 TO EURM-ROUNDING-AMT
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 5.00 TO EURM-ROUNDING-AMT
               WHEN OTHER
                   COMPUTE EURM-ROUNDING-AMT ROUNDED = EURM-FEE-AMT *
                       EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-ROUNDING-AMT > 0 AND EURM-ROUNDING-AMT < 2.00
               MOVE 2.00 TO EURM-ROUNDING-AMT
           END-IF
           IF EURM-ROUNDING-AMT > 35.00
               MOVE 35.00 TO EURM-ROUNDING-AMT
           END-IF
           ADD EURM-ROUNDING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E808 NAME REQUIRED
      * E996 FEE AMT NEGATIVE
      * E343 FEE AMT OVER 50000
      * E752 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN EURT-FEE-AMT < 0
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN EURT-FEE-AMT > 50000.00
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN EURM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E752' TO WS-REJECT-CODE
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
       3050-DESCRIBE-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NATIONAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3060-FLAG-NATIONAL.
           IF EURM-NATIONAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-NATIONAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E406 CUST ID REQUIRED
      * E710 ROUNDING YTD NEGATIVE
      * E432 ROUNDING YTD OVER 5000
      * E125 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CUST-ID = SPACES
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD < 0
                   MOVE 'E710' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD > 5000.00
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN EURM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN MATURITY DATE IS PAST
      * RV -> AD WHEN MATURITY DATE IS PAST
      * AD -> RD WHEN MATURITY DATE IS PAST
      * RD -> FX WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-TRAN.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E979 DESC REQUIRED
      * E935 CONVERTED AMT NEGATIVE
      * E476 CONVERTED AMT OVER 99999
      * E544 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-DESC = SPACES
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-AMT < 0
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-AMT > 99999.00
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN EURM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-CONVERTED.
           IF EURT-CONVERTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-CONVERTED-AMT * EURM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROUNDING TIER TIER FOR THE ROUNDING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ROUNDING-TIER-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURM-ROUNDING-YTD NOT < WS-ROUNDING-TIER-LOW(WS-TX)
                   MOVE WS-ROUNDING-TIER-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO EURM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E517 NAME REQUIRED
      * E276 TRIANGULAR YTD NEGATIVE
      * E725 TRIANGULAR YTD OVER 50000
      * E090 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-YTD < 0
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-YTD > 50000.00
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN EURT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED FEE ON THE TRIANGULAR AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3150-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO EURT-CONVERTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EURT-CONVERTED-AMT ROUNDED =
                       EURM-TRIANGULAR-AMT * EURM-FEE-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 3.00 TO EURT-CONVERTED-AMT
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 20.00 TO EURT-CONVERTED-AMT
               WHEN OTHER
                   COMPUTE EURT-CONVERTED-AMT ROUNDED =
                       EURM-TRIANGULAR-AMT * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURT-CONVERTED-AMT > 0 AND EURT-CONVERTED-AMT < 2.00
               MOVE 2.00 TO EURT-CONVERTED-AMT
           END-IF
           IF EURT-CONVERTED-AMT > 250.00
               MOVE 250.00 TO EURT-CONVERTED-AMT
           END-IF
           ADD EURT-CONVERTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3160-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EURM-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   COMPUTE EURM-NATIONAL-AMT ROUNDED = WS-WORK-AMT *
                       EURM-FEE-PCT / 200
               WHEN EURM-CONV-METHOD-CD = 'GL'
                   MOVE 12.00 TO EURM-NATIONAL-AMT
               WHEN EURM-CONV-METHOD-CD = 'JA'
                   MOVE 5.00 TO EURM-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EURM-NATIONAL-AMT ROUNDED = WS-WORK-AMT *
                       EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-NATIONAL-AMT > 0 AND EURM-NATIONAL-AMT < 2.00
               MOVE 2.00 TO EURM-NATIONAL-AMT
           END-IF
           IF EURM-NATIONAL-AMT > 35.00
               MOVE 35.00 TO EURM-NATIONAL-AMT
           END-IF
           ADD EURM-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RESIDUAL AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-RESIDUAL.
           IF EURM-RESIDUAL-AMT < 50.00
               MOVE 50.00 TO EURM-RESIDUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-RESIDUAL-AMT > 10000.00
               MOVE 10000.00 TO EURM-RESIDUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-GRID-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EURT-FEE-AMT NOT < WS-EURO-GRID-LOW(WS-TX)
                   MOVE WS-EURO-GRID-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-MATURITY.
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
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN OPEN DATE IS PAST
      * JA -> LD WHEN OPEN DATE IS PAST
      * LD -> MR WHEN OPEN DATE IS PAST
      * MR -> VC WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * B -> F WHEN OPEN DATE IS PAST
      * F -> K WHEN OPEN DATE IS PAST
      * K -> R WHEN OPEN DATE IS PAST
      * R -> U WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO EURM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO EURM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO EURM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO EURM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E016 REF NO REQUIRED
      * E966 ROUNDING AMT NEGATIVE
      * E448 ROUNDING AMT OVER 1000000
      * E191 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-REF-NO = SPACES
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN EURT-ROUNDING-AMT < 0
                   MOVE 'E966' TO WS-REJECT-CODE
               WHEN EURT-ROUNDING-AMT > 1000000.00
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN EURM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO EURM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED YTD FEE ON THE NATIONAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3260-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EURM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-CONVERTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURM-CONVERTED-YTD ROUNDED =
                       EURT-NATIONAL-AMT * EURM-FEE-PCT / 200
               WHEN EURM-CONV-METHOD-CD = 'GL'
                   MOVE 20.00 TO EURM-CONVERTED-YTD
               WHEN EURM-CONV-METHOD-CD = 'JA'
                   MOVE 15.00 TO EURM-CONVERTED-YTD
               WHEN OTHER
                   COMPUTE EURM-CONVERTED-YTD ROUNDED =
                       EURT-NATIONAL-AMT * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-CONVERTED-YTD > 0 AND EURM-CONVERTED-YTD < 1.00
               MOVE 1.00 TO EURM-CONVERTED-YTD
           END-IF
           IF EURM-CONVERTED-YTD > 25.00
               MOVE 25.00 TO EURM-CONVERTED-YTD
           END-IF
           ADD EURM-CONVERTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ROUNDING TIER TIER FOR THE TRIANGULAR AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROUNDING-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURM-TRIANGULAR-AMT NOT < WS-ROUNDING-TIER-LOW(WS-TX)
                   MOVE WS-ROUNDING-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EURO AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-TRAN-CD
                   ADD EURM-EURO-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-EURO-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CONV METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ROUNDING AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3310-FLAG-ROUNDING.
           IF EURT-ROUNDING-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURT-ROUNDING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E224 ADDR REQUIRED
      * E745 TRIANGULAR AMT NEGATIVE
      * E949 TRIANGULAR AMT OVER 99999
      * E069 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-ADDR = SPACES
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT < 0
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT > 99999.00
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN EURT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN NEXT DUE DATE IS PAST
      * JA -> LD WHEN NEXT DUE DATE IS PAST
      * LD -> MR WHEN NEXT DUE DATE IS PAST
      * MR -> VC WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EURT-CONVERTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURT-CONVERTED-AMT - (WS-WORK-AMT *
               3)
           COMPUTE EURT-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-NATIONAL.
           IF EURT-NATIONAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-NATIONAL-AMT * EURM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONVERTED FEE ON THE FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3380-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EURM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-CONVERTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   COMPUTE EURM-CONVERTED-AMT ROUNDED = EURT-FEE-AMT *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-STATUS-CD = 'B'
                   MOVE 12.00 TO EURM-CONVERTED-AMT
               WHEN EURM-STATUS-CD = 'F'
                   MOVE 10.00 TO EURM-CONVERTED-AMT
               WHEN OTHER
                   COMPUTE EURM-CONVERTED-AMT ROUNDED = EURT-FEE-AMT *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-CONVERTED-AMT > 0 AND EURM-CONVERTED-AMT < 1.00
               MOVE 1.00 TO EURM-CONVERTED-AMT
           END-IF
           IF EURM-CONVERTED-AMT > 250.00
               MOVE 250.00 TO EURM-CONVERTED-AMT
           END-IF
           ADD EURM-CONVERTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-REGION-CD
                   ADD EURM-NATIONAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-NATIONAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EURT-NATIONAL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURT-NATIONAL-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EURT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROUNDING TIER TIER FOR THE ROUNDING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROUNDING-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURM-ROUNDING-YTD NOT < WS-ROUNDING-TIER-LOW(WS-TX)
                   MOVE WS-ROUNDING-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3430-FLAG-NATIONAL.
           IF EURT-NATIONAL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURT-NATIONAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E498 CONTACT REQUIRED
      * E125 FEE AMT NEGATIVE
      * E976 FEE AMT OVER 25000
      * E606 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN EURT-FEE-AMT < 0
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN EURT-FEE-AMT > 25000.00
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN EURM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E060 REJECT CD REQUIRED
      * E799 WORK AMT NEGATIVE
      * E176 WORK AMT OVER 250000
      * E208 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-REJECT-CD = SPACES
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN EURM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN NEXT DUE DATE IS PAST
      * RV -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> RD WHEN NEXT DUE DATE IS PAST
      * RD -> FX WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-EFFECTIVE.
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
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ROUNDING TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROUNDING-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURT-FEE-AMT NOT < WS-ROUNDING-TIER-LOW(WS-TX)
                   MOVE WS-ROUNDING-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN LAST STMT DATE IS PAST
      * EBM -> GAU WHEN LAST STMT DATE IS PAST
      * GAU -> GSU WHEN LAST STMT DATE IS PAST
      * GSU -> VWP WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EURT-CONVERTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURT-CONVERTED-AMT - (WS-WORK-AMT *
               3)
           COMPUTE EURM-CONVERTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3540-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD < 50.00
               MOVE 50.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-YTD > 500.00
               MOVE 500.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROUNDING TIER TIER FOR THE RESIDUAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROUNDING-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURM-RESIDUAL-YTD NOT < WS-ROUNDING-TIER-LOW(WS-TX)
                   MOVE WS-ROUNDING-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * B -> F WHEN NEXT DUE DATE IS PAST
      * F -> K WHEN NEXT DUE DATE IS PAST
      * K -> R WHEN NEXT DUE DATE IS PAST
      * R -> U WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO EURM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO EURM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO EURM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO EURM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-REGION-CD
                   ADD EURT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROUNDING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3590-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-FEE-YTD ROUNDED =
                   EURM-ROUNDING-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-FEE-YTD
           END-IF
           ADD EURM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-MATURITY.
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
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN NEXT DUE DATE IS PAST
      * SMF -> UEC WHEN NEXT DUE DATE IS PAST
      * UEC -> VJA WHEN NEXT DUE DATE IS PAST
      * VJA -> WPH WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-TYPE.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E665 NAME REQUIRED
      * E517 NATIONAL AMT NEGATIVE
      * E819 NATIONAL AMT OVER 250000
      * E126 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN EURT-NATIONAL-AMT < 0
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN EURT-NATIONAL-AMT > 250000.00
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN EURM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN EFFECTIVE DATE IS PAST
      * JA -> LD WHEN EFFECTIVE DATE IS PAST
      * LD -> MR WHEN EFFECTIVE DATE IS PAST
      * MR -> VC WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3640-LIMIT-FEE.
           IF EURM-FEE-AMT < 0.00
               MOVE 0.00 TO EURM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-FEE-AMT > 500.00
               MOVE 500.00 TO EURM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 10
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E750 DESC REQUIRED
      * E302 ROUNDING YTD NEGATIVE
      * E257 ROUNDING YTD OVER 1000000
      * E292 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-DESC = SPACES
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD < 0
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD > 1000000.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN EURM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE RESIDUAL YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3670-ASSESS-STOP.
           EVALUATE TRUE
               WHEN EURM-STOP-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED =
                       EURM-RESIDUAL-YTD * EURM-PENALTY-PCT / 200
               WHEN EURM-STATUS-CD = 'B'
                   MOVE 3.00 TO EURM-TRIANGULAR-YTD
               WHEN EURM-STATUS-CD = 'F'
                   MOVE 7.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED =
                       EURM-RESIDUAL-YTD * EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 2.00
               MOVE 2.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 150.00
               MOVE 150.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3690-CHECK-MATURITY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO EURM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-FEE.
           IF EURM-FEE-AMT < 50.00
               MOVE 50.00 TO EURM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-FEE-AMT > 50000.00
               MOVE 50000.00 TO EURM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR AMT BY CURRENCY CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CURRENCY-CD
                   ADD EURM-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-NATIONAL.
           IF EURM-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-NATIONAL-YTD * EURM-RATE / 100 / 360
               ADD WS-WORK-AMT TO EURM-TRIANGULAR-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-EURO.
           IF EURM-EURO-AMT < 25.00
               MOVE 25.00 TO EURM-EURO-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-EURO-AMT > 1000.00
               MOVE 1000.00 TO EURM-EURO-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CONV METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURM-ROUNDING-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURM-ROUNDING-AMT - (WS-WORK-AMT * 2)
           COMPUTE EURM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURM-ROUNDING-YTD / 12
           COMPUTE WS-WORK-AMT-2 = EURM-ROUNDING-YTD - (WS-WORK-AMT *
               12)
           COMPUTE EURM-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EURM-NATIONAL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EURM-NATIONAL-YTD - (WS-WORK-AMT * 3)
           COMPUTE EURM-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN LAST PAY DATE IS PAST
      * JA -> LD WHEN LAST PAY DATE IS PAST
      * LD -> MR WHEN LAST PAY DATE IS PAST
      * MR -> VC WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF EURT-POST-DATE > 0 AND EURT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED AMT BY CURRENCY CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CURRENCY-CD
                   ADD EURM-CONVERTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-CONVERTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EURM-NATIONAL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-NATIONAL-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EURM-ROUNDING-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3860-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE EURM-TRIANGULAR-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3870-FLAG-EURO.
           IF EURM-EURO-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-EURO-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3880-FLAG-NATIONAL.
           IF EURM-NATIONAL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-NATIONAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-FEE.
           IF EURT-FEE-AMT < 50.00
               MOVE 50.00 TO EURT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-FEE-AMT > 50000.00
               MOVE 50000.00 TO EURT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN NOTICE DATE IS PAST
      * JA -> LD WHEN NOTICE DATE IS PAST
      * LD -> MR WHEN NOTICE DATE IS PAST
      * MR -> VC WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO GRID TIER FOR THE NATIONAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EURT-NATIONAL-AMT NOT < WS-EURO-GRID-LOW(WS-TX)
                   MOVE WS-EURO-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EURO AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-EURO.
           IF EURM-EURO-AMT < 10.00
               MOVE 10.00 TO EURM-EURO-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-EURO-AMT > 10000.00
               MOVE 10000.00 TO EURM-EURO-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3940-FLAG-FEE.
           IF EURM-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E264 REJECT CD REQUIRED
      * E826 RESIDUAL YTD NEGATIVE
      * E240 RESIDUAL YTD OVER 250000
      * E825 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-REJECT-CD = SPACES
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-YTD < 0
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-YTD > 250000.00
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN EURM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E825' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-NATIONAL.
           IF EURM-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-NATIONAL-YTD * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-OPEN.
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
               MOVE 'Y' TO EURM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EURO YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-EURO.
           IF EURM-EURO-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-EURO-YTD * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-RESIDUAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-ROUNDING.
           IF EURM-ROUNDING-AMT < 0.00
               MOVE 0.00 TO EURM-ROUNDING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-ROUNDING-AMT > 5000.00
               MOVE 5000.00 TO EURM-ROUNDING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4020-LIMIT-CONVERTED.
           IF EURM-CONVERTED-AMT < 25.00
               MOVE 25.00 TO EURM-CONVERTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-CONVERTED-AMT > 50000.00
               MOVE 50000.00 TO EURM-CONVERTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-LAST-MAINT.
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
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE NATIONAL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4050-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = EURM-NATIONAL-AMT *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = EURM-NATIONAL-AMT *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-FEE.
           IF EURM-FEE-YTD < 0.00
               MOVE 0.00 TO EURM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-FEE-YTD > 10000.00
               MOVE 10000.00 TO EURM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO EURM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E267 DESC REQUIRED
      * E637 EURO YTD NEGATIVE
      * E813 EURO YTD OVER 99999
      * E082 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-DESC = SPACES
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD < 0
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD > 99999.00
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN EURM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESIDUAL YTD.
      *----------------------------------------------------------------*
       4090-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-YTD ROUNDED =
                   EURT-CONVERTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-YTD
           END-IF
           ADD EURM-RESIDUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE EURO GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-GRID-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EURM-FEE-AMT NOT < WS-EURO-GRID-LOW(WS-TX)
                   MOVE WS-EURO-GRID-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-EXCEPT.
           IF EURM-EXCEPT-CNT < 9999999
               ADD 1 TO EURM-EXCEPT-CNT
           END-IF
           IF EURM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-NEXT-REVIEW.
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
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EURM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4150-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-YTD - (WS-WORK-AMT *
               3)
           COMPUTE EURM-EURO-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CONV METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CONV-METHOD-CD
                   ADD EURM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING YTD FEE ON THE CONVERTED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4170-ASSESS-STOP.
           EVALUATE TRUE
               WHEN EURM-STOP-FLAG = 'Y'
                   MOVE ZERO TO EURM-ROUNDING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EURM-ROUNDING-YTD ROUNDED =
                       EURM-CONVERTED-AMT * EURM-FEE-PCT / 200
               WHEN EURM-REGION-CD = 'DTL'
                   MOVE 10.00 TO EURM-ROUNDING-YTD
               WHEN EURM-REGION-CD = 'HYD'
                   MOVE 20.00 TO EURM-ROUNDING-YTD
               WHEN OTHER
                   COMPUTE EURM-ROUNDING-YTD ROUNDED =
                       EURM-CONVERTED-AMT * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-ROUNDING-YTD > 0 AND EURM-ROUNDING-YTD < 2.00
               MOVE 2.00 TO EURM-ROUNDING-YTD
           END-IF
           IF EURM-ROUNDING-YTD > 100.00
               MOVE 100.00 TO EURM-ROUNDING-YTD
           END-IF
           ADD EURM-ROUNDING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-FEE.
           IF EURM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-FEE-YTD * EURM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EURO YTD.
      *----------------------------------------------------------------*
       4190-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-YTD ROUNDED =
                   EURM-CONVERTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-EURO-YTD
           END-IF
           ADD EURM-EURO-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-YTD - (WS-WORK-AMT *
               4)
           COMPUTE EURM-CONVERTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4220-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EURM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO EURM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4230-FLAG-NATIONAL.
           IF EURM-NATIONAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-NATIONAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING FEE ON THE CONVERTED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4250-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EURM-ROUNDING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   COMPUTE EURM-ROUNDING-AMT ROUNDED =
                       EURM-CONVERTED-AMT * EURM-RATE / 200
               WHEN EURM-TYPE-CD = 'AGL'
                   MOVE 10.00 TO EURM-ROUNDING-AMT
               WHEN EURM-TYPE-CD = 'SMF'
                   MOVE 12.00 TO EURM-ROUNDING-AMT
               WHEN OTHER
                   COMPUTE EURM-ROUNDING-AMT ROUNDED =
                       EURM-CONVERTED-AMT * EURM-RATE / 100
           END-EVALUATE
           IF EURM-ROUNDING-AMT > 0 AND EURM-ROUNDING-AMT < 2.00
               MOVE 2.00 TO EURM-ROUNDING-AMT
           END-IF
           IF EURM-ROUNDING-AMT > 100.00
               MOVE 100.00 TO EURM-ROUNDING-AMT
           END-IF
           ADD EURM-ROUNDING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4260-AGE-EFFECTIVE.
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
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-ROUNDING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4270-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-YTD - (WS-WORK-AMT *
               3)
           COMPUTE EURM-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-AMT - (WS-WORK-AMT *
               4)
           COMPUTE EURM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE EUR FIELDS TO EUPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-EUPRC.
           INITIALIZE EUPRC-PARMS
           MOVE EURM-EURO-AMT
             TO EUPRC-EURO-AMT
           MOVE EURM-FEE-AMT
             TO EUPRC-FEE-AMT
           MOVE EURM-ROUNDING-YTD
             TO EUPRC-ROUNDING-YTD
           MOVE EURM-EURO-YTD
             TO EUPRC-EURO-YTD
           MOVE EURM-CONVERTED-YTD
             TO EUPRC-CONVERTED-YTD
           MOVE EURM-CONVERTED-AMT
             TO EUPRC-CONVERTED-AMT
           MOVE EURM-LAST-MAINT-DATE
             TO EUPRC-LAST-MAINT-DATE
           MOVE EURM-NEXT-REVIEW-DATE
             TO EUPRC-NEXT-REVIEW-DATE
           MOVE EURM-LAST-PAY-DATE
             TO EUPRC-LAST-PAY-DATE
           MOVE EURM-LAST-STMT-DATE
             TO EUPRC-LAST-STMT-DATE
           MOVE EURM-CURRENCY-CD
             TO EUPRC-CURRENCY-CD
           MOVE EURM-CONV-METHOD-CD
             TO EUPRC-CONV-METHOD-CD
           MOVE EURM-TYPE-CD
             TO EUPRC-TYPE-CD
           MOVE EURM-CHANNEL-CD
             TO EUPRC-CHANNEL-CD
           MOVE EURM-REVIEW-FLAG
             TO EUPRC-REVIEW-FLAG
           MOVE EURM-AUTO-RENEW-FLAG
             TO EUPRC-AUTO-RENEW-FLAG
           MOVE EURM-OVERRIDE-FLAG
             TO EUPRC-OVERRIDE-FLAG
           MOVE EURM-CYCLE-CNT
             TO EUPRC-CYCLE-CNT
           MOVE EURM-EXCEPT-CNT
             TO EUPRC-EXCEPT-CNT
           MOVE EURM-RATE
             TO EUPRC-RATE
           MOVE EURM-PENALTY-PCT
             TO EUPRC-PENALTY-PCT
           MOVE EURM-DESC
             TO EUPRC-DESC
           MOVE EURM-CONTACT
             TO EUPRC-CONTACT
           CALL 'EUPRC' USING EUPRC-PARMS
           EVALUATE EUPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE EUPRC-FEE-AMT
                     TO EURM-FEE-AMT
                   MOVE EUPRC-ROUNDING-YTD
                     TO EURM-ROUNDING-YTD
                   MOVE EUPRC-EURO-YTD
                     TO EURM-EURO-YTD
                   MOVE EUPRC-REVIEW-FLAG
                     TO EURM-REVIEW-FLAG
                   MOVE EUPRC-AUTO-RENEW-FLAG
                     TO EURM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-EUPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE EUVAL-TRIANGULAR-YTD
                     TO EURM-TRIANGULAR-YTD
                   MOVE EUVAL-CONVERTED-AMT
                     TO EURM-CONVERTED-AMT
                   MOVE EUVAL-TRIANGULAR-AMT
                     TO EURM-TRIANGULAR-AMT
                   MOVE EUVAL-RESIDUAL-YTD
                     TO EURM-RESIDUAL-YTD
                   MOVE EUVAL-OVERRIDE-FLAG
                     TO EURM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-EUVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE EUR FIELDS TO EUFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-EUFEE.
           INITIALIZE EUFEE-PARMS
           MOVE EURM-CONVERTED-YTD
             TO EUFEE-CONVERTED-YTD
           MOVE EURM-NATIONAL-YTD
             TO EUFEE-NATIONAL-YTD
           MOVE EURM-NATIONAL-AMT
             TO EUFEE-NATIONAL-AMT
           MOVE EURM-TRIANGULAR-YTD
             TO EUFEE-TRIANGULAR-YTD
           MOVE EURM-EURO-YTD
             TO EUFEE-EURO-YTD
           MOVE EURM-FEE-YTD
             TO EUFEE-FEE-YTD
           MOVE EURM-LAST-MAINT-DATE
             TO EUFEE-LAST-MAINT-DATE
           MOVE EURM-NOTICE-DATE
             TO EUFEE-NOTICE-DATE
           MOVE EURM-EFFECTIVE-DATE
             TO EUFEE-EFFECTIVE-DATE
           MOVE EURM-LAST-PAY-DATE
             TO EUFEE-LAST-PAY-DATE
           MOVE EURM-CONV-METHOD-CD
             TO EUFEE-CONV-METHOD-CD
           MOVE EURM-REGION-CD
             TO EUFEE-REGION-CD
           MOVE EURM-TYPE-CD
             TO EUFEE-TYPE-CD
           MOVE EURM-CHANNEL-CD
             TO EUFEE-CHANNEL-CD
           MOVE EURM-PREFERRED-FLAG
             TO EUFEE-PREFERRED-FLAG
           MOVE EURM-OVERRIDE-FLAG
             TO EUFEE-OVERRIDE-FLAG
           MOVE EURM-REVIEW-FLAG
             TO EUFEE-REVIEW-FLAG
           MOVE EURM-CYCLE-CNT
             TO EUFEE-CYCLE-CNT
           MOVE EURM-PAYMENT-CNT
             TO EUFEE-PAYMENT-CNT
           MOVE EURM-FEE-PCT
             TO EUFEE-FEE-PCT
           MOVE EURM-RATE
             TO EUFEE-RATE
           MOVE EURM-REF-NO
             TO EUFEE-REF-NO
           MOVE EURM-CUST-ID
             TO EUFEE-CUST-ID
           CALL 'EUFEE' USING EUFEE-PARMS
           EVALUATE EUFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE EUFEE-CONVERTED-YTD
                     TO EURM-CONVERTED-YTD
                   MOVE EUFEE-NATIONAL-AMT
                     TO EURM-NATIONAL-AMT
                   MOVE EUFEE-TRIANGULAR-YTD
                     TO EURM-TRIANGULAR-YTD
                   MOVE EUFEE-EURO-YTD
                     TO EURM-EURO-YTD
                   MOVE EUFEE-REVIEW-FLAG
                     TO EURM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-EUFEE' TO ABND-PARAGRAPH
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
           DISPLAY 'EURSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'EURSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
