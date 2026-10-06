      *================================================================*
      * PROGRAM  : EURSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EURO CONVERSION - SETTLEMENT. READS THE EUR         *
      *            TRANSACTIONS, UPDATES THE EUR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. NATIONAL CURRENCY BALANCE       *
      *            CONVERSION TO EURO AND DUAL DISPLAY                 *
      * INPUT    : EURMST (KSDS), EURTRN, BANKCTL                      *
      * OUTPUT   : EUROUT, EURSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, EUVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-03-22 LMB  ORIGINAL                                       *
      * 1999-07-15 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-12-26 MKL  ROUNDING CALCULATION CORRECTED INC6544         *
      * 2003-10-24 LMB  FEE CALCULATION CORRECTED REQ0853              *
      * 2003-09-19 EWS  FEE NOTICE TEXT PER LEGAL CHG8093              *
      * 2008-11-12 LMB  CONVERTED TABLE EXPANDED PRB9414               *
      * 2010-06-12 PQR  PERFORMANCE - TRIANGULAR READ AHEAD INC7143    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EURSETL.
       AUTHOR. J L MARSH.

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
           SELECT REPORT-FILE ASSIGN TO "EURSERPT"
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
                                           VALUE 19.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
                                           VALUE '00000000000000891500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000386500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000869000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000206000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008050000000071500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008300000000850000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009300000000294500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014300000000736000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024300000000673500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024400000000443500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025400000000876500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025900000000671500'.
       01  WS-TRIANGUL-TIER-TBL REDEFINES WS-TRIANGUL-TIER-VALUES.
           05  WS-TRIANGUL-TIER-ROW OCCURS 12 TIMES.
               10  WS-TRIANGUL-TIER-LOW    PIC 9(09)V99.
               10  WS-TRIANGUL-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONVERTE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONVERTE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000460500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000394000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000839000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000652000'.
       01  WS-CONVERTE-GRID-TBL REDEFINES WS-CONVERTE-GRID-VALUES.
           05  WS-CONVERTE-GRID-ROW OCCURS 4 TIMES.
               10  WS-CONVERTE-GRID-LOW    PIC 9(09)V99.
               10  WS-CONVERTE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TRIANGUL-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRIANGUL-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000132000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000262500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000777500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000363000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008050000000126500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000388000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000597000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000166000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024050000000824000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024550000000784000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024600000000155500'.
       01  WS-TRIANGUL-GRID-TBL REDEFINES WS-TRIANGUL-GRID-VALUES.
           05  WS-TRIANGUL-GRID-ROW OCCURS 11 TIMES.
               10  WS-TRIANGUL-GRID-LOW    PIC 9(09)V99.
               10  WS-TRIANGUL-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  EURSETL  EURO CONVERSION'.
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
               PERFORM 3000-CHECK-LAST-PAY
               PERFORM 3010-ASSESS-OVERRIDE
               PERFORM 3020-SPLIT-FEE
               PERFORM 3030-CHECK-EFF
               PERFORM 3040-AGE-LAST-STMT
               PERFORM 3050-SELECT-PENALTY-PCT
               PERFORM 3060-SELECT-FEE-PCT
               PERFORM 3070-PRORATE-ROUNDING
               PERFORM 3080-PRORATE-ROUNDING
               PERFORM 3090-COUNT-SEQ
               PERFORM 3100-DESCRIBE-REGION
               PERFORM 3110-ACCRUE-NATIONAL
               PERFORM 3120-PRORATE-RESIDUAL
               PERFORM 3130-CHECK-LAST-MAINT
               PERFORM 3140-CHECK-OPEN
               PERFORM 3150-ACCRUE-FEE
               PERFORM 3160-ACCRUE-CONVERTED
               PERFORM 3170-DESCRIBE-CHANNEL
               PERFORM 3180-PRORATE-ROUNDING
               PERFORM 3190-ACCRUE-EURO
               PERFORM 3200-PRORATE-CONVERTED
               PERFORM 3210-CHECK-EFF
               PERFORM 3220-CHECK-LAST-PAY
               PERFORM 3230-ASSESS-PREFERRED
               PERFORM 3240-ASSESS-AUTO-RENEW
               PERFORM 3250-COUNT-CYCLE
               PERFORM 3260-PRORATE-ROUNDING
               PERFORM 3270-ASSESS-OVERRIDE
               PERFORM 3280-PRORATE-ROUNDING
               PERFORM 3290-PRORATE-EURO
               PERFORM 3300-ACCUM-STATUS
               PERFORM 3310-PRORATE-RESIDUAL
               PERFORM 3320-ASSESS-EXEMPT
               PERFORM 3330-AGE-MATURITY
               PERFORM 3340-LIMIT-RESIDUAL
               PERFORM 3350-DESCRIBE-CHANNEL
               PERFORM 3360-EDIT-NAME
               PERFORM 3370-SELECT-RATE
               PERFORM 3380-PRORATE-NATIONAL
               PERFORM 3390-CHECK-LAST-PAY
               PERFORM 3400-FLAG-TRIANGULAR
               PERFORM 3410-DESCRIBE-STATUS
               PERFORM 3420-EDIT-MEMO
               PERFORM 3430-AGE-NEXT-REVIEW
               PERFORM 3440-SET-REGION
               PERFORM 3450-AGE-POST
               PERFORM 3460-EDIT-REF-NO
               PERFORM 3470-ACCUM-TYPE
               PERFORM 3480-LIMIT-ROUNDING
               PERFORM 3490-DESCRIBE-TYPE
               PERFORM 3500-PRORATE-ROUNDING
               PERFORM 3510-PRORATE-CONVERTED
               PERFORM 3520-EDIT-REJECT
               PERFORM 3530-DESCRIBE-REGION
               PERFORM 3540-CHECK-NEXT-DUE
               PERFORM 3550-DESCRIBE-STATUS
               PERFORM 3560-EDIT-REF-NO
               PERFORM 3570-LIMIT-FEE
               PERFORM 3580-FLAG-NATIONAL
               PERFORM 3590-SET-REGION
               PERFORM 3600-ASSESS-PREFERRED
               PERFORM 3610-EDIT-ADDR
               PERFORM 3620-SET-STATUS
               PERFORM 3630-ACCRUE-CONVERTED
               PERFORM 3640-SPLIT-CONVERTED
               PERFORM 3650-SELECT-PENALTY-PCT
               PERFORM 3660-AGE-EFFECTIVE
               PERFORM 3670-SPLIT-EURO
               PERFORM 3680-SELECT-FEE-PCT
               PERFORM 3690-ACCRUE-RESIDUAL
               PERFORM 3700-DESCRIBE-TYPE
               PERFORM 3710-AGE-LAST-STMT
               PERFORM 3720-LIMIT-ROUNDING
               PERFORM 3730-SPLIT-RESIDUAL
               PERFORM 3740-SET-TRAN
               PERFORM 3750-CHECK-POST
               PERFORM 3760-ASSESS-AUTO-RENEW
               PERFORM 3770-ASSESS-PREFERRED
               PERFORM 3780-CHECK-OPEN
               PERFORM 3790-CHECK-MATURITY
               PERFORM 3800-CHECK-NOTICE
               PERFORM 3810-SET-CHANNEL
               PERFORM 3820-COUNT-PAYMENT
               PERFORM 3830-EDIT-NAME
               PERFORM 3840-ACCRUE-CONVERTED
               PERFORM 3850-AGE-POST
               PERFORM 3860-ACCRUE-FEE
               PERFORM 3870-SELECT-FEE-PCT
               PERFORM 3880-SET-CURRENCY
               PERFORM 3890-EDIT-ADDR
               PERFORM 3900-SELECT-RATE
               PERFORM 3910-AGE-MATURITY
               PERFORM 3920-SPLIT-CONVERTED
               PERFORM 3930-LIMIT-ROUNDING
               PERFORM 3940-DESCRIBE-STATUS
               PERFORM 3950-DESCRIBE-STATUS
               PERFORM 3960-SELECT-FEE-PCT
               PERFORM 3970-ACCUM-REGION
               PERFORM 3980-LIMIT-NATIONAL
               PERFORM 3990-SPLIT-ROUNDING
               PERFORM 4000-DESCRIBE-CURRENCY
               PERFORM 4010-FLAG-RESIDUAL
               PERFORM 4020-CHECK-NEXT-DUE
               PERFORM 4030-ACCUM-TRAN
               PERFORM 4040-ACCUM-TYPE
               PERFORM 4050-CHECK-NOTICE
               PERFORM 4060-EDIT-NAME
               PERFORM 4070-ACCRUE-NATIONAL
               PERFORM 4080-DESCRIBE-CONV-METHOD
               PERFORM 4090-EDIT-CONTACT
               PERFORM 4100-SPLIT-FEE
               PERFORM 4110-COUNT-CYCLE
               PERFORM 4120-ASSESS-AUTO-RENEW
               PERFORM 4130-ACCUM-TYPE
               PERFORM 4140-COUNT-EXCEPT
               PERFORM 4150-ACCRUE-PENALTY-PCT
               PERFORM 4160-SELECT-PENALTY-PCT
               PERFORM 4170-SET-CURRENCY
               PERFORM 4180-DESCRIBE-TYPE
               PERFORM 4190-EDIT-MEMO
               PERFORM 4200-SELECT-FEE-PCT
               PERFORM 4210-SET-REGION
               PERFORM 4220-CHECK-EFF
               PERFORM 4230-LIMIT-CONVERTED
               PERFORM 4240-AGE-MATURITY
               PERFORM 4250-DESCRIBE-CURRENCY
               PERFORM 4260-ACCRUE-FEE
               PERFORM 4270-ACCRUE-EURO
               PERFORM 4280-COUNT-EXCEPT
               PERFORM 4290-AGE-LAST-STMT
               PERFORM 4300-ACCUM-CURRENCY
               PERFORM 4310-ACCUM-STATUS
               PERFORM 4320-ACCRUE-PENALTY-PCT
               PERFORM 4330-SET-TYPE
               PERFORM 4340-EDIT-ADDR
               PERFORM 4350-CHECK-NEXT-REVIEW
               PERFORM 4360-ACCUM-CHANNEL
               PERFORM 4370-EDIT-CUST-ID
               PERFORM 4380-ACCUM-CONV-METHOD
               PERFORM 4390-EDIT-CUST-ID
               PERFORM 4400-CHECK-POST
               PERFORM 4410-SELECT-RATE
               PERFORM 4420-SELECT-FEE-PCT
               PERFORM 4430-EDIT-CONTACT
               PERFORM 4440-LIMIT-ROUNDING
               PERFORM 4450-SPLIT-CONVERTED
               PERFORM 4460-ACCRUE-NATIONAL
               PERFORM 4470-PRORATE-RESIDUAL
               PERFORM 4480-ACCUM-CHANNEL
               PERFORM 4490-CHECK-LAST-STMT
               PERFORM 4500-AGE-EFFECTIVE
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
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-LAST-PAY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO EURM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE EURO YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EURM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURM-FEE-YTD ROUNDED = EURM-EURO-YTD *
                       EURM-FEE-PCT / 200
               WHEN EURT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO EURM-FEE-YTD
               WHEN EURT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO EURM-FEE-YTD
               WHEN OTHER
                   COMPUTE EURM-FEE-YTD ROUNDED = EURM-EURO-YTD *
                       EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-FEE-YTD > 0 AND EURM-FEE-YTD < 5.00
               MOVE 5.00 TO EURM-FEE-YTD
           END-IF
           IF EURM-FEE-YTD > 75.00
               MOVE 75.00 TO EURM-FEE-YTD
           END-IF
           ADD EURM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF EURT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURT-EFF-DATE TO DTU-DATE-1
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
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-LAST-STMT.
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
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE ROUNDING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EURM-ROUNDING-YTD NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EURM-FEE-YTD NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       3070-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURT-ROUNDING-AMT ROUNDED =
                   EURM-EURO-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURT-ROUNDING-AMT
           END-IF
           ADD EURT-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ROUNDING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROUNDING YTD.
      *----------------------------------------------------------------*
       3080-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-ROUNDING-YTD ROUNDED =
                   EURT-ROUNDING-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-ROUNDING-YTD
           END-IF
           ADD EURM-ROUNDING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 5
               MOVE 'Y' TO EURM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-NATIONAL.
           IF EURT-NATIONAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-NATIONAL-AMT * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURM-TRIANGULAR-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESIDUAL AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-AMT ROUNDED =
                   EURT-CONVERTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-FEE.
           IF EURM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-FEE-YTD * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURM-TRIANGULAR-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-CONVERTED.
           IF EURM-CONVERTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-CONVERTED-AMT * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE NATIONAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-ROUNDING-AMT ROUNDED =
                   EURM-NATIONAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-ROUNDING-AMT
           END-IF
           ADD EURM-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EURO AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-EURO.
           IF EURM-EURO-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-EURO-AMT * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURT-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NATIONAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONVERTED AMT.
      *----------------------------------------------------------------*
       3200-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURT-CONVERTED-AMT ROUNDED =
                   EURM-NATIONAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURT-CONVERTED-AMT
           END-IF
           ADD EURT-CONVERTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF EURT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURT-EFF-DATE TO DTU-DATE-1
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
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-LAST-PAY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO EURT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EURO FEE ON THE TRIANGULAR YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3230-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EURM-EURO-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   COMPUTE EURM-EURO-AMT ROUNDED = EURM-TRIANGULAR-YTD *
                       EURM-FEE-PCT / 200
               WHEN EURM-REGION-CD = 'DTL'
                   MOVE 10.00 TO EURM-EURO-AMT
               WHEN EURM-REGION-CD = 'HYD'
                   MOVE 10.00 TO EURM-EURO-AMT
               WHEN OTHER
                   COMPUTE EURM-EURO-AMT ROUNDED = EURM-TRIANGULAR-YTD *
                       EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-EURO-AMT > 0 AND EURM-EURO-AMT < 10.00
               MOVE 10.00 TO EURM-EURO-AMT
           END-IF
           IF EURM-EURO-AMT > 150.00
               MOVE 150.00 TO EURM-EURO-AMT
           END-IF
           ADD EURM-EURO-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE NATIONAL AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3240-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   COMPUTE EURM-FEE-YTD ROUNDED = EURT-NATIONAL-AMT *
                       EURM-FEE-PCT / 200
               WHEN EURM-CHANNEL-CD = 'AEA'
                   MOVE 7.00 TO EURM-FEE-YTD
               WHEN EURM-CHANNEL-CD = 'EBM'
                   MOVE 20.00 TO EURM-FEE-YTD
               WHEN OTHER
                   COMPUTE EURM-FEE-YTD ROUNDED = EURT-NATIONAL-AMT *
                       EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-FEE-YTD > 0 AND EURM-FEE-YTD < 10.00
               MOVE 10.00 TO EURM-FEE-YTD
           END-IF
           IF EURM-FEE-YTD > 50.00
               MOVE 50.00 TO EURM-FEE-YTD
           END-IF
           ADD EURM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       3260-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURT-ROUNDING-AMT ROUNDED =
                   EURM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURT-ROUNDING-AMT
           END-IF
           ADD EURT-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RESIDUAL YTD FEE ON THE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3270-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EURM-RESIDUAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   COMPUTE EURM-RESIDUAL-YTD ROUNDED = EURM-FEE-AMT *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-STATUS-CD = 'B'
                   MOVE 3.00 TO EURM-RESIDUAL-YTD
               WHEN EURM-STATUS-CD = 'F'
                   MOVE 15.00 TO EURM-RESIDUAL-YTD
               WHEN OTHER
                   COMPUTE EURM-RESIDUAL-YTD ROUNDED = EURM-FEE-AMT *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-RESIDUAL-YTD > 0 AND EURM-RESIDUAL-YTD < 5.00
               MOVE 5.00 TO EURM-RESIDUAL-YTD
           END-IF
           IF EURM-RESIDUAL-YTD > 100.00
               MOVE 100.00 TO EURM-RESIDUAL-YTD
           END-IF
           ADD EURM-RESIDUAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING YTD.
      *----------------------------------------------------------------*
       3280-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-ROUNDING-YTD ROUNDED =
                   EURM-TRIANGULAR-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-ROUNDING-YTD
           END-IF
           ADD EURM-ROUNDING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EURO AMT.
      *----------------------------------------------------------------*
       3290-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-AMT ROUNDED =
                   EURT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-EURO-AMT
           END-IF
           ADD EURM-EURO-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-STATUS-CD
                   ADD EURM-ROUNDING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-ROUNDING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROUNDING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESIDUAL YTD.
      *----------------------------------------------------------------*
       3310-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-YTD ROUNDED =
                   EURM-ROUNDING-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-YTD
           END-IF
           ADD EURM-RESIDUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE RESIDUAL YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3320-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EURT-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   COMPUTE EURT-NATIONAL-AMT ROUNDED = EURM-RESIDUAL-YTD
                       * EURM-FEE-PCT / 200
               WHEN EURM-CHANNEL-CD = 'AEA'
                   MOVE 3.00 TO EURT-NATIONAL-AMT
               WHEN EURM-CHANNEL-CD = 'EBM'
                   MOVE 7.00 TO EURT-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EURT-NATIONAL-AMT ROUNDED = EURM-RESIDUAL-YTD
                       * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURT-NATIONAL-AMT > 0 AND EURT-NATIONAL-AMT < 1.00
               MOVE 1.00 TO EURT-NATIONAL-AMT
           END-IF
           IF EURT-NATIONAL-AMT > 25.00
               MOVE 25.00 TO EURT-NATIONAL-AMT
           END-IF
           ADD EURT-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 15
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESIDUAL YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3340-LIMIT-RESIDUAL.
           IF EURM-RESIDUAL-YTD < 25.00
               MOVE 25.00 TO EURM-RESIDUAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-RESIDUAL-YTD > 2500.00
               MOVE 2500.00 TO EURM-RESIDUAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E363 NAME REQUIRED
      * E908 FEE YTD NEGATIVE
      * E779 FEE YTD OVER 99999
      * E296 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN EURM-FEE-YTD < 0
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN EURM-FEE-YTD > 99999.00
                   MOVE 'E779' TO WS-REJECT-CODE
               WHEN EURM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE EURO YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EURM-EURO-YTD NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROUNDING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-NATIONAL-AMT ROUNDED =
                   EURM-ROUNDING-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-NATIONAL-AMT
           END-IF
           ADD EURM-NATIONAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-LAST-PAY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO EURM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3400-FLAG-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E202 MEMO REQUIRED
      * E873 ROUNDING AMT NEGATIVE
      * E875 ROUNDING AMT OVER 10000
      * E844 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-MEMO = SPACES
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN EURT-ROUNDING-AMT < 0
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN EURT-ROUNDING-AMT > 10000.00
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN EURM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E844' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-NEXT-REVIEW.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN MATURITY DATE IS PAST
      * HYD -> JUL WHEN MATURITY DATE IS PAST
      * JUL -> WSE WHEN MATURITY DATE IS PAST
      * WSE -> YAU WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-POST.
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
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E906 REF NO REQUIRED
      * E651 CONVERTED AMT NEGATIVE
      * E439 CONVERTED AMT OVER 10000
      * E113 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-REF-NO = SPACES
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-AMT < 0
                   MOVE 'E651' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-AMT > 10000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN EURM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-TYPE-CD
                   ADD EURM-CONVERTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-CONVERTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3480-LIMIT-ROUNDING.
           IF EURT-ROUNDING-AMT < 50.00
               MOVE 50.00 TO EURT-ROUNDING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-ROUNDING-AMT > 500.00
               MOVE 500.00 TO EURT-ROUNDING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RESIDUAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING YTD.
      *----------------------------------------------------------------*
       3500-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-ROUNDING-YTD ROUNDED =
                   EURM-RESIDUAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-ROUNDING-YTD
           END-IF
           ADD EURM-ROUNDING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONVERTED AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-CONVERTED-AMT ROUNDED =
                   EURM-TRIANGULAR-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-CONVERTED-AMT
           END-IF
           ADD EURM-CONVERTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E292 REJECT CD REQUIRED
      * E353 TRIANGULAR AMT NEGATIVE
      * E963 TRIANGULAR AMT OVER 99999
      * E850 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-REJECT-CD = SPACES
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT < 0
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT > 99999.00
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN EURM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E850' TO WS-REJECT-CODE
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
       3530-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E349 REF NO REQUIRED
      * E891 RESIDUAL AMT NEGATIVE
      * E478 RESIDUAL AMT OVER 250000
      * E745 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3560-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-REF-NO = SPACES
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-AMT < 0
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-AMT > 250000.00
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN EURM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3570-LIMIT-FEE.
           IF EURT-FEE-AMT < 100.00
               MOVE 100.00 TO EURT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-FEE-AMT > 500.00
               MOVE 500.00 TO EURT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3580-FLAG-NATIONAL.
           IF EURM-NATIONAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-NATIONAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN MATURITY DATE IS PAST
      * HYD -> JUL WHEN MATURITY DATE IS PAST
      * JUL -> WSE WHEN MATURITY DATE IS PAST
      * WSE -> YAU WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING FEE ON THE NATIONAL AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3600-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EURT-ROUNDING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-REVIEW-FLAG = 'Y'
                   COMPUTE EURT-ROUNDING-AMT ROUNDED = EURM-NATIONAL-AMT
                       * EURM-RATE / 200
               WHEN EURM-CONV-METHOD-CD = 'GL'
                   MOVE 15.00 TO EURT-ROUNDING-AMT
               WHEN EURM-CONV-METHOD-CD = 'JA'
                   MOVE 7.00 TO EURT-ROUNDING-AMT
               WHEN OTHER
                   COMPUTE EURT-ROUNDING-AMT ROUNDED = EURM-NATIONAL-AMT
                       * EURM-RATE / 100
           END-EVALUATE
           IF EURT-ROUNDING-AMT > 0 AND EURT-ROUNDING-AMT < 2.00
               MOVE 2.00 TO EURT-ROUNDING-AMT
           END-IF
           IF EURT-ROUNDING-AMT > 100.00
               MOVE 100.00 TO EURT-ROUNDING-AMT
           END-IF
           ADD EURT-ROUNDING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E954 ADDR REQUIRED
      * E509 TRIANGULAR YTD NEGATIVE
      * E918 TRIANGULAR YTD OVER 1000000
      * E884 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-ADDR = SPACES
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-YTD < 0
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-YTD > 1000000.00
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN EURT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * B -> F WHEN NEXT DUE DATE IS PAST
      * F -> K WHEN NEXT DUE DATE IS PAST
      * K -> R WHEN NEXT DUE DATE IS PAST
      * R -> U WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-STATUS.
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
      * DAILY INTEREST ON CONVERTED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-CONVERTED.
           IF EURM-CONVERTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-CONVERTED-AMT * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURT-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EURM-CONVERTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-CONVERTED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EURM-EURO-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE ROUNDING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EURM-ROUNDING-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-EFFECTIVE.
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
                   ADD EURM-RESIDUAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-RESIDUAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-RESIDUAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-RESIDUAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-RESIDUAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT EURO AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-AMT - (WS-WORK-AMT * 12)
           COMPUTE EURT-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE RESIDUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EURM-RESIDUAL-AMT NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESIDUAL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-RESIDUAL.
           IF EURM-RESIDUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-RESIDUAL-AMT * EURM-RATE / 100 / 360
               ADD WS-WORK-AMT TO EURM-EURO-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-LAST-STMT.
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
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ROUNDING AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3720-LIMIT-ROUNDING.
           IF EURT-ROUNDING-AMT < 10.00
               MOVE 10.00 TO EURT-ROUNDING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-ROUNDING-AMT > 2500.00
               MOVE 2500.00 TO EURT-ROUNDING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESIDUAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-RESIDUAL.
           COMPUTE WS-WORK-AMT = EURM-RESIDUAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURM-RESIDUAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE EURT-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN NEXT REVIEW DATE IS PAST
      * RV -> AD WHEN NEXT REVIEW DATE IS PAST
      * AD -> RD WHEN NEXT REVIEW DATE IS PAST
      * RD -> FX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   IF EURM-NEXT-REVIEW-DATE > 0 AND
                       EURM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RV' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF EURM-NEXT-REVIEW-DATE > 0 AND
                       EURM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF EURM-NEXT-REVIEW-DATE > 0 AND
                       EURM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF EURM-NEXT-REVIEW-DATE > 0 AND
                       EURM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FX' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-POST.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE NATIONAL AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3760-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-REVIEW-FLAG = 'Y'
                   COMPUTE EURM-NATIONAL-AMT ROUNDED = EURT-NATIONAL-AMT
                       * EURM-FEE-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 10.00 TO EURM-NATIONAL-AMT
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 12.00 TO EURM-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EURM-NATIONAL-AMT ROUNDED = EURT-NATIONAL-AMT
                       * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-NATIONAL-AMT > 0 AND EURM-NATIONAL-AMT < 5.00
               MOVE 5.00 TO EURM-NATIONAL-AMT
           END-IF
           IF EURM-NATIONAL-AMT > 75.00
               MOVE 75.00 TO EURM-NATIONAL-AMT
           END-IF
           ADD EURM-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE EURO AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3770-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = EURM-EURO-AMT *
                       EURM-RATE / 200
               WHEN EURM-CHANNEL-CD = 'AEA'
                   MOVE 10.00 TO EURM-TRIANGULAR-YTD
               WHEN EURM-CHANNEL-CD = 'EBM'
                   MOVE 12.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = EURM-EURO-AMT *
                       EURM-RATE / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 2.00
               MOVE 2.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 75.00
               MOVE 75.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-OPEN.
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
               MOVE 'Y' TO EURM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-MATURITY.
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
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-NOTICE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO EURM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN MATURITY DATE IS PAST
      * EBM -> GAU WHEN MATURITY DATE IS PAST
      * GAU -> GSU WHEN MATURITY DATE IS PAST
      * GSU -> VWP WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E939 NAME REQUIRED
      * E316 ROUNDING YTD NEGATIVE
      * E127 ROUNDING YTD OVER 250000
      * E404 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD < 0
                   MOVE 'E316' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD > 250000.00
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN EURM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E404' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-CONVERTED.
           IF EURM-CONVERTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-CONVERTED-YTD * EURM-RATE / 100 / 360
               ADD WS-WORK-AMT TO EURT-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-POST.
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
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-FEE.
           IF EURT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-FEE-AMT * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-RESIDUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL GRID TIER FOR THE ROUNDING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-ROUNDING-AMT NOT < WS-TRIANGUL-GRID-LOW(WS-TX)
                   MOVE WS-TRIANGUL-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * EW -> FJ WHEN NEXT DUE DATE IS PAST
      * FJ -> JC WHEN NEXT DUE DATE IS PAST
      * JC -> EW WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF EURM-NEXT-DUE-DATE > 0 AND EURM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EW' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E846 ADDR REQUIRED
      * E516 NATIONAL YTD NEGATIVE
      * E526 NATIONAL YTD OVER 50000
      * E403 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-ADDR = SPACES
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN EURM-NATIONAL-YTD < 0
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN EURM-NATIONAL-YTD > 50000.00
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN EURM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E403' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE EURO YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3900-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
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
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 15
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE EURT-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-ROUNDING.
           IF EURT-ROUNDING-AMT < 25.00
               MOVE 25.00 TO EURT-ROUNDING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-ROUNDING-AMT > 50000.00
               MOVE 50000.00 TO EURT-ROUNDING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EURT-CONVERTED-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EURO AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-REGION-CD
                   ADD EURM-EURO-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-EURO-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-NATIONAL.
           IF EURM-NATIONAL-YTD < 100.00
               MOVE 100.00 TO EURM-NATIONAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-NATIONAL-YTD > 500.00
               MOVE 500.00 TO EURM-NATIONAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURM-ROUNDING-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-ROUNDING-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EURT-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESIDUAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4010-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO EURM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-TRAN-CD
                   ADD EURM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-TYPE-CD
                   ADD EURM-TRIANGULAR-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-TRIANGULAR-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4050-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO EURM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E390 NAME REQUIRED
      * E146 NATIONAL AMT NEGATIVE
      * E281 NATIONAL AMT OVER 10000
      * E944 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN EURT-NATIONAL-AMT < 0
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN EURT-NATIONAL-AMT > 10000.00
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN EURM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-NATIONAL.
           IF EURM-NATIONAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-NATIONAL-AMT * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURT-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CONV METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E648 CONTACT REQUIRED
      * E548 TRIANGULAR AMT NEGATIVE
      * E463 TRIANGULAR AMT OVER 99999
      * E513 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E648' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT < 0
                   MOVE 'E548' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT > 99999.00
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN EURM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURT-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURT-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE EURT-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ROUNDING YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4120-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EURM-ROUNDING-YTD *
                       EURM-RATE / 200
               WHEN EURT-TRAN-CD = 'CV'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN EURT-TRAN-CD = 'RV'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EURM-ROUNDING-YTD *
                       EURM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE EURO YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-TYPE-CD
                   ADD EURM-EURO-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-EURO-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-EXCEPT.
           IF EURM-EXCEPT-CNT < 9999999
               ADD 1 TO EURM-EXCEPT-CNT
           END-IF
           IF EURM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EURM-CONVERTED-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * EW -> FJ WHEN LAST MAINT DATE IS PAST
      * FJ -> JC WHEN LAST MAINT DATE IS PAST
      * JC -> EW WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JC' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EW' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E719 MEMO REQUIRED
      * E244 FEE YTD NEGATIVE
      * E505 FEE YTD OVER 250000
      * E199 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-MEMO = SPACES
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN EURM-FEE-YTD < 0
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN EURM-FEE-YTD > 250000.00
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN EURM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL GRID TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURT-CONVERTED-AMT NOT < WS-TRIANGUL-GRID-LOW(WS-TX)
                   MOVE WS-TRIANGUL-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN LAST STMT DATE IS PAST
      * HYD -> JUL WHEN LAST STMT DATE IS PAST
      * JUL -> WSE WHEN LAST STMT DATE IS PAST
      * WSE -> YAU WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4220-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF EURT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO EURM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4230-LIMIT-CONVERTED.
           IF EURT-CONVERTED-AMT < 0.00
               MOVE 0.00 TO EURT-CONVERTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-CONVERTED-AMT > 500.00
               MOVE 500.00 TO EURT-CONVERTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4240-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 60
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4260-ACCRUE-FEE.
           IF EURM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-FEE-AMT * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURT-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EURO AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-EURO.
           IF EURM-EURO-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-EURO-AMT * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4280-COUNT-EXCEPT.
           IF EURM-EXCEPT-CNT < 9999999
               ADD 1 TO EURM-EXCEPT-CNT
           END-IF
           IF EURM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-LAST-STMT.
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
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4300-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CURRENCY-CD
                   ADD EURT-NATIONAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURT-NATIONAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESIDUAL AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-STATUS-CD
                   ADD EURM-RESIDUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-RESIDUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4320-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-NATIONAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN LAST MAINT DATE IS PAST
      * SMF -> UEC WHEN LAST MAINT DATE IS PAST
      * UEC -> VJA WHEN LAST MAINT DATE IS PAST
      * VJA -> WPH WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EURM-LAST-MAINT-DATE > 0 AND EURM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E781 ADDR REQUIRED
      * E625 FEE AMT NEGATIVE
      * E003 FEE AMT OVER 10000
      * E791 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4340-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-ADDR = SPACES
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN EURT-FEE-AMT < 0
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN EURT-FEE-AMT > 10000.00
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN EURT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO EURT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CHANNEL-CD
                   ADD EURM-CONVERTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-CONVERTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E962 CUST ID REQUIRED
      * E212 WORK AMT NEGATIVE
      * E739 WORK AMT OVER 99999
      * E247 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CUST-ID = SPACES
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN EURM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESIDUAL YTD BY CONV METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4380-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CONV-METHOD-CD
                   ADD EURM-RESIDUAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-RESIDUAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E040 CUST ID REQUIRED
      * E109 ROUNDING YTD NEGATIVE
      * E772 ROUNDING YTD OVER 250000
      * E089 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4390-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CUST-ID = SPACES
                   MOVE 'E040' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD < 0
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD > 250000.00
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN EURM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4400-CHECK-POST.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO EURT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4410-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EURT-CONVERTED-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE EURO AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4420-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF EURM-EURO-AMT NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E119 CONTACT REQUIRED
      * E099 TRIANGULAR AMT NEGATIVE
      * E115 TRIANGULAR AMT OVER 50000
      * E300 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4430-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT < 0
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT > 50000.00
                   MOVE 'E115' TO WS-REJECT-CODE
               WHEN EURM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E300' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4440-LIMIT-ROUNDING.
           IF EURM-ROUNDING-AMT < 0.00
               MOVE 0.00 TO EURM-ROUNDING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-ROUNDING-AMT > 1000.00
               MOVE 1000.00 TO EURM-ROUNDING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EURM-CONVERTED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EURM-CONVERTED-YTD - (WS-WORK-AMT *
               3)
           COMPUTE EURT-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4460-ACCRUE-NATIONAL.
           IF EURM-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-NATIONAL-YTD * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-CONVERTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESIDUAL AMT.
      *----------------------------------------------------------------*
       4470-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-AMT ROUNDED =
                   EURT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE EURO YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4480-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CHANNEL-CD
                   ADD EURM-EURO-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-EURO-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4490-CHECK-LAST-STMT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO EURM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 30
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-AMT TO WS-AGE-AMT(5)
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
                   MOVE EUVAL-NATIONAL-AMT
                     TO EURM-NATIONAL-AMT
                   MOVE EUVAL-CONVERTED-AMT
                     TO EURM-CONVERTED-AMT
                   MOVE EUVAL-PREFERRED-FLAG
                     TO EURM-PREFERRED-FLAG
                   MOVE EUVAL-EXEMPT-FLAG
                     TO EURM-EXEMPT-FLAG
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
           DISPLAY 'EURSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'EURSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
