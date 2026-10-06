      *================================================================*
      * PROGRAM  : EUREDIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EURO CONVERSION - INPUT EDIT. READS THE EUR         *
      *            TRANSACTIONS, UPDATES THE EUR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. NATIONAL CURRENCY BALANCE       *
      *            CONVERSION TO EURO AND DUAL DISPLAY                 *
      * INPUT    : EURMST (KSDS), EURTRN, BANKCTL                      *
      * OUTPUT   : EUROUT, EUREDRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, EUVAL, EUELG, EUPRC     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-08-11 MKL  ORIGINAL                                       *
      * 1998-02-22 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-06 BHS  RESIDUAL CALCULATION CORRECTED PRB9448         *
      * 2000-06-15 SKN  RESIDUAL STATUS CODES ADDED REQ8329            *
      * 2001-11-20 PQR  RESIDUAL TABLE EXPANDED REQ6591                *
      * 2001-12-10 JLM  ROUNDING EXCEPTIONS TO OPS QUEUE CHG5413       *
      * 2001-07-06 RJT  ROUNDING ROUNDING - HALF UP CHG4164            *
      * 2001-08-08 SKN  TRIANGULAR CALCULATION CORRECTED PRB0347       *
      * 2004-03-24 BHS  NATIONAL CALCULATION CORRECTED CHG0504         *
      * 2004-07-10 SKN  NATIONAL CALCULATION CORRECTED CHG8624         *
      * 2006-10-23 RJT  FEE FEE SCHEDULE UPDATE REQ0130                *
      * 2010-01-25 GAF  FEE ROUNDING - HALF UP INC5157                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUREDIT.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "EUREDRPT"
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
                                           VALUE 14.000000.
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
      * WS-TRIANGUL-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRIANGUL-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000373000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000273500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000350000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000490500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000733500'.
       01  WS-TRIANGUL-BAND-TBL REDEFINES WS-TRIANGUL-BAND-VALUES.
           05  WS-TRIANGUL-BAND-ROW OCCURS 5 TIMES.
               10  WS-TRIANGUL-BAND-LOW    PIC 9(09)V99.
               10  WS-TRIANGUL-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TRIANGUL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TRIANGUL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000377500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000873000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000808000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000719000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000237000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020750000000716000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000271000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021300000000133000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000575000'.
       01  WS-TRIANGUL-TIER-TBL REDEFINES WS-TRIANGUL-TIER-VALUES.
           05  WS-TRIANGUL-TIER-ROW OCCURS 9 TIMES.
               10  WS-TRIANGUL-TIER-LOW    PIC 9(09)V99.
               10  WS-TRIANGUL-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONVERTE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONVERTE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000078500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000221000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000603500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000844500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000667500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011200000000243500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000755000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013800000000357000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014800000000560000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017300000000643000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017400000000184500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017900000000880000'.
       01  WS-CONVERTE-GRID-TBL REDEFINES WS-CONVERTE-GRID-VALUES.
           05  WS-CONVERTE-GRID-ROW OCCURS 12 TIMES.
               10  WS-CONVERTE-GRID-LOW    PIC 9(09)V99.
               10  WS-CONVERTE-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  EUREDIT  EURO CONVERSION'.
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
       COPY EUELGP.
       COPY EUPRCP.

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
               PERFORM 3000-ACCUM-CONV-METHOD
               PERFORM 3010-AGE-NOTICE
               PERFORM 3020-FLAG-RESIDUAL
               PERFORM 3030-PRORATE-RESIDUAL
               PERFORM 3040-EDIT-DESC
               PERFORM 3050-LIMIT-EURO
               PERFORM 3060-ACCRUE-RESIDUAL
               PERFORM 3070-LIMIT-EURO
               PERFORM 3080-ACCUM-TYPE
               PERFORM 3090-EDIT-REJECT
               PERFORM 3100-EDIT-MEMO
               PERFORM 3110-COUNT-CYCLE
               PERFORM 3120-LIMIT-ROUNDING
               PERFORM 3130-DESCRIBE-CHANNEL
               PERFORM 3140-COUNT-ITEM
               PERFORM 3150-SET-CHANNEL
               PERFORM 3160-AGE-EFF
               PERFORM 3170-ACCUM-CHANNEL
               PERFORM 3180-CHECK-LAST-MAINT
               PERFORM 3190-COUNT-PAYMENT
               PERFORM 3200-LIMIT-CONVERTED
               PERFORM 3210-LIMIT-NATIONAL
               PERFORM 3220-FLAG-ITEM
               PERFORM 3230-EDIT-NAME
               PERFORM 3240-SELECT-PENALTY-PCT
               PERFORM 3250-COUNT-PAYMENT
               PERFORM 3260-ASSESS-STOP
               PERFORM 3270-AGE-EFF
               PERFORM 3280-DESCRIBE-TYPE
               PERFORM 3290-LIMIT-CONVERTED
               PERFORM 3300-AGE-NEXT-REVIEW
               PERFORM 3310-EDIT-MEMO
               PERFORM 3320-CHECK-EFF
               PERFORM 3330-SET-TYPE
               PERFORM 3340-SELECT-PENALTY-PCT
               PERFORM 3350-ASSESS-REVERSAL
               PERFORM 3360-FLAG-RESIDUAL
               PERFORM 3370-ASSESS-EXEMPT
               PERFORM 3380-EDIT-NAME
               PERFORM 3390-CHECK-NEXT-DUE
               PERFORM 3400-ACCRUE-RESIDUAL
               PERFORM 3410-SELECT-PENALTY-PCT
               PERFORM 3420-CHECK-NEXT-REVIEW
               PERFORM 3430-ASSESS-PREFERRED
               PERFORM 3440-COUNT-SEQ
               PERFORM 3450-CHECK-LAST-STMT
               PERFORM 3460-SELECT-RATE
               PERFORM 3470-SELECT-PENALTY-PCT
               PERFORM 3480-EDIT-REJECT
               PERFORM 3490-ACCRUE-CONVERTED
               PERFORM 3500-SPLIT-TRIANGULAR
               PERFORM 3510-SPLIT-TRIANGULAR
               PERFORM 3520-ACCUM-TYPE
               PERFORM 3530-FLAG-FEE
               PERFORM 3540-FLAG-TRIANGULAR
               PERFORM 3550-SELECT-FEE-PCT
               PERFORM 3560-FLAG-ITEM
               PERFORM 3570-SELECT-RATE
               PERFORM 3580-ACCRUE-ROUNDING
               PERFORM 3590-PRORATE-EURO
               PERFORM 3600-FLAG-FEE
               PERFORM 3610-SET-REGION
               PERFORM 3620-ACCUM-CURRENCY
               PERFORM 3630-DESCRIBE-CHANNEL
               PERFORM 3640-ASSESS-AUTO-RENEW
               PERFORM 3650-CHECK-NEXT-DUE
               PERFORM 3660-SET-TRAN
               PERFORM 3670-ACCRUE-FEE
               PERFORM 3680-PRORATE-ROUNDING
               PERFORM 3690-EDIT-ADDR
               PERFORM 3700-COUNT-SEQ
               PERFORM 3710-CHECK-NEXT-DUE
               PERFORM 3720-ACCUM-CONV-METHOD
               PERFORM 3730-EDIT-MEMO
               PERFORM 3740-AGE-OPEN
               PERFORM 3750-ACCUM-CONV-METHOD
               PERFORM 3760-SET-REGION
               PERFORM 3770-CHECK-LAST-MAINT
               PERFORM 3780-AGE-OPEN
               PERFORM 3790-EDIT-CONTACT
               PERFORM 3800-SELECT-FEE-PCT
               PERFORM 3810-COUNT-ITEM
               PERFORM 3820-FLAG-RESIDUAL
               PERFORM 3830-SELECT-RATE
               PERFORM 3840-COUNT-CYCLE
               PERFORM 3850-PRORATE-EURO
               PERFORM 3860-SET-TYPE
               PERFORM 3870-SET-TRAN
               PERFORM 3880-LIMIT-NATIONAL
               PERFORM 3890-COUNT-PAYMENT
               PERFORM 3900-ACCRUE-CONVERTED
               PERFORM 3910-SPLIT-FEE
               PERFORM 3920-EDIT-REF-NO
               PERFORM 3930-EDIT-ADDR
               PERFORM 3940-FLAG-ROUNDING
               PERFORM 3950-SELECT-FEE-PCT
               PERFORM 3960-SELECT-PENALTY-PCT
               PERFORM 3970-FLAG-NATIONAL
               PERFORM 3980-ACCRUE-FEE
               PERFORM 3990-FLAG-RESIDUAL
               PERFORM 4000-PRORATE-FEE
               PERFORM 4010-AGE-NEXT-REVIEW
               PERFORM 4020-FLAG-ITEM
               PERFORM 4030-SET-CONV-METHOD
               PERFORM 4040-DESCRIBE-CHANNEL
               PERFORM 4050-SET-CONV-METHOD
               PERFORM 4060-SPLIT-TRIANGULAR
               PERFORM 4070-COUNT-ITEM
               PERFORM 4080-ASSESS-AUTO-RENEW
               PERFORM 4090-PRORATE-NATIONAL
               PERFORM 4100-CHECK-NOTICE
               PERFORM 4110-LIMIT-TRIANGULAR
               PERFORM 4120-SET-CHANNEL
               PERFORM 4130-FLAG-ROUNDING
               PERFORM 4140-COUNT-EXCEPT
               PERFORM 4150-DESCRIBE-TRAN
               PERFORM 4160-COUNT-PAYMENT
               PERFORM 4170-AGE-EFF
               PERFORM 4180-PRORATE-NATIONAL
               PERFORM 4190-AGE-LAST-STMT
               PERFORM 4200-SELECT-RATE
               PERFORM 4210-SET-CURRENCY
               PERFORM 4220-PRORATE-RESIDUAL
               PERFORM 4230-LIMIT-FEE
               PERFORM 4240-COUNT-EXCEPT
               PERFORM 4250-SELECT-PENALTY-PCT
               PERFORM 4260-COUNT-PAYMENT
               PERFORM 4270-AGE-EFF
               PERFORM 4280-LIMIT-RESIDUAL
               PERFORM 4290-EDIT-CONTACT
               PERFORM 4300-FLAG-FEE
               PERFORM 4310-SPLIT-ROUNDING
               PERFORM 4320-SELECT-FEE-PCT
               PERFORM 4330-LIMIT-TRIANGULAR
               PERFORM 4340-LIMIT-ROUNDING
               PERFORM 4350-AGE-POST
               PERFORM 4360-COUNT-SEQ
               PERFORM 4370-AGE-NEXT-DUE
               PERFORM 4380-SET-CURRENCY
               PERFORM 4390-ACCRUE-NATIONAL
               PERFORM 4400-SELECT-PENALTY-PCT
               PERFORM 4410-EDIT-REJECT
               PERFORM 4420-LIMIT-ROUNDING
               PERFORM 4430-DESCRIBE-CURRENCY
               PERFORM 4440-ACCRUE-ROUNDING
               PERFORM 4450-SPLIT-NATIONAL
               PERFORM 4460-ACCRUE-NATIONAL
               PERFORM 4470-FLAG-CONVERTED
               PERFORM 4480-ACCUM-REGION
               PERFORM 4490-PRORATE-EURO
               PERFORM 4500-FLAG-RESIDUAL
               PERFORM 4510-ASSESS-OVERRIDE
               PERFORM 4520-SELECT-FEE-PCT
               PERFORM 4530-PRORATE-CONVERTED
               PERFORM 4540-SELECT-FEE-PCT
               PERFORM 4550-EDIT-MEMO
               PERFORM 4560-DESCRIBE-TYPE
               PERFORM 4570-AGE-LAST-MAINT
               PERFORM 4580-ACCRUE-FEE
               PERFORM 4590-CHECK-POST
               PERFORM 4600-ASSESS-PREFERRED
               PERFORM 4610-ACCRUE-RESIDUAL
               PERFORM 4620-DESCRIBE-TRAN
               PERFORM 7040-CALL-EUVAL
               PERFORM 7000-CALL-EUELG
               PERFORM 7010-CALL-EUPRC
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
      * ACCUMULATE CONVERTED AMT BY CONV METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CONV-METHOD-CD
                   ADD EURM-CONVERTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-CONVERTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
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
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESIDUAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3020-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NATIONAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESIDUAL AMT.
      *----------------------------------------------------------------*
       3030-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-AMT ROUNDED =
                   EURM-NATIONAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E241 DESC REQUIRED
      * E360 CONVERTED AMT NEGATIVE
      * E696 CONVERTED AMT OVER 99999
      * E521 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-DESC = SPACES
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN EURT-CONVERTED-AMT < 0
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN EURT-CONVERTED-AMT > 99999.00
                   MOVE 'E696' TO WS-REJECT-CODE
               WHEN EURM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EURO YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-EURO.
           IF EURM-EURO-YTD < 10.00
               MOVE 10.00 TO EURM-EURO-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-EURO-YTD > 10000.00
               MOVE 10000.00 TO EURM-EURO-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESIDUAL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-RESIDUAL-YTD * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-EURO.
           IF EURM-EURO-YTD < 50.00
               MOVE 50.00 TO EURM-EURO-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-EURO-YTD > 5000.00
               MOVE 5000.00 TO EURM-EURO-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-TYPE-CD
                   ADD EURT-ROUNDING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURT-ROUNDING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E375 REJECT CD REQUIRED
      * E009 ROUNDING AMT NEGATIVE
      * E670 ROUNDING AMT OVER 99999
      * E516 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-REJECT-CD = SPACES
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-AMT < 0
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-AMT > 99999.00
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN EURT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E556 MEMO REQUIRED
      * E260 WORK AMT NEGATIVE
      * E763 WORK AMT OVER 99999
      * E765 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-MEMO = SPACES
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN EURM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3120-LIMIT-ROUNDING.
           IF EURM-ROUNDING-YTD < 10.00
               MOVE 10.00 TO EURM-ROUNDING-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-ROUNDING-YTD > 1000.00
               MOVE 1000.00 TO EURM-ROUNDING-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 3
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN OPEN DATE IS PAST
      * EBM -> GAU WHEN OPEN DATE IS PAST
      * GAU -> GSU WHEN OPEN DATE IS PAST
      * GSU -> VWP WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-CHANNEL.
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
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-EFF.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-CHANNEL-CD
                   ADD EURM-CONVERTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-CONVERTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-LAST-MAINT.
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
               MOVE 'Y' TO EURM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-CONVERTED.
           IF EURM-CONVERTED-YTD < 50.00
               MOVE 50.00 TO EURM-CONVERTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-CONVERTED-YTD > 10000.00
               MOVE 10000.00 TO EURM-CONVERTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3210-LIMIT-NATIONAL.
           IF EURM-NATIONAL-AMT < 100.00
               MOVE 100.00 TO EURM-NATIONAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-NATIONAL-AMT > 2500.00
               MOVE 2500.00 TO EURM-NATIONAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3220-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E995 NAME REQUIRED
      * E691 WORK AMT 2 NEGATIVE
      * E889 WORK AMT 2 OVER 50000
      * E460 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN EURM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-BAND-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF EURM-FEE-AMT NOT < WS-TRIANGUL-BAND-LOW(WS-TX)
                   MOVE WS-TRIANGUL-BAND-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE RESIDUAL YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3260-ASSESS-STOP.
           EVALUATE TRUE
               WHEN EURM-STOP-FLAG = 'Y'
                   MOVE ZERO TO EURT-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-REVIEW-FLAG = 'Y'
                   COMPUTE EURT-NATIONAL-AMT ROUNDED = EURM-RESIDUAL-YTD
                       * EURM-FEE-PCT / 200
               WHEN EURM-REGION-CD = 'DTL'
                   MOVE 5.00 TO EURT-NATIONAL-AMT
               WHEN EURM-REGION-CD = 'HYD'
                   MOVE 10.00 TO EURT-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EURT-NATIONAL-AMT ROUNDED = EURM-RESIDUAL-YTD
                       * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURT-NATIONAL-AMT > 0 AND EURT-NATIONAL-AMT < 5.00
               MOVE 5.00 TO EURT-NATIONAL-AMT
           END-IF
           IF EURT-NATIONAL-AMT > 50.00
               MOVE 50.00 TO EURT-NATIONAL-AMT
           END-IF
           ADD EURT-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
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
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONVERTED AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3290-LIMIT-CONVERTED.
           IF EURM-CONVERTED-AMT < 50.00
               MOVE 50.00 TO EURM-CONVERTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-CONVERTED-AMT > 5000.00
               MOVE 5000.00 TO EURM-CONVERTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-NEXT-REVIEW.
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
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 MEMO REQUIRED
      * E856 ROUNDING AMT NEGATIVE
      * E496 ROUNDING AMT OVER 5000
      * E867 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-MEMO = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-AMT < 0
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-AMT > 5000.00
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN EURM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-EFF.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO EURM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN OPEN DATE IS PAST
      * SMF -> UEC WHEN OPEN DATE IS PAST
      * UEC -> VJA WHEN OPEN DATE IS PAST
      * VJA -> WPH WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EURM-TYPE-CD
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
       3340-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-EURO-YTD NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3350-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = EURM-FEE-AMT *
                       EURM-RATE / 200
               WHEN EURT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO EURM-TRIANGULAR-YTD
               WHEN EURT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = EURM-FEE-AMT *
                       EURM-RATE / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 2.00
               MOVE 2.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 150.00
               MOVE 150.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RESIDUAL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3360-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE EURO YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3370-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EURM-EURO-YTD *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-CHANNEL-CD = 'AEA'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN EURM-CHANNEL-CD = 'EBM'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = EURM-EURO-YTD *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E052 NAME REQUIRED
      * E184 EURO YTD NEGATIVE
      * E689 EURO YTD OVER 99999
      * E857 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-NAME = SPACES
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD < 0
                   MOVE 'E184' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD > 99999.00
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN EURM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO EURM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESIDUAL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3400-ACCRUE-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-RESIDUAL-YTD * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURT-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE RESIDUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EURM-RESIDUAL-AMT NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-NEXT-REVIEW.
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
      * ASSESS THE CONVERTED YTD FEE ON THE CONVERTED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3430-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EURM-CONVERTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-REVIEW-FLAG = 'Y'
                   COMPUTE EURM-CONVERTED-YTD ROUNDED =
                       EURT-CONVERTED-AMT * EURM-FEE-PCT / 200
               WHEN EURT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO EURM-CONVERTED-YTD
               WHEN EURT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO EURM-CONVERTED-YTD
               WHEN OTHER
                   COMPUTE EURM-CONVERTED-YTD ROUNDED =
                       EURT-CONVERTED-AMT * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-CONVERTED-YTD > 0 AND EURM-CONVERTED-YTD < 2.00
               MOVE 2.00 TO EURM-CONVERTED-YTD
           END-IF
           IF EURM-CONVERTED-YTD > 50.00
               MOVE 50.00 TO EURM-CONVERTED-YTD
           END-IF
           ADD EURM-CONVERTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 5
               MOVE 'Y' TO EURM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-LAST-STMT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL BAND TIER FOR THE ROUNDING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-BAND-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF EURM-ROUNDING-AMT NOT < WS-TRIANGUL-BAND-LOW(WS-TX)
                   MOVE WS-TRIANGUL-BAND-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE RESIDUAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-RESIDUAL-YTD NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E253 REJECT CD REQUIRED
      * E486 RESIDUAL AMT NEGATIVE
      * E675 RESIDUAL AMT OVER 1000000
      * E809 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-REJECT-CD = SPACES
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-AMT < 0
                   MOVE 'E486' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-AMT > 1000000.00
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN EURM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E809' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-CONVERTED.
           IF EURM-CONVERTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-CONVERTED-YTD * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURM-RESIDUAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EURT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-YTD - (WS-WORK-AMT *
               4)
           COMPUTE EURT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3520-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3530-FLAG-FEE.
           IF EURM-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-BAND-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF EURT-FEE-AMT NOT < WS-TRIANGUL-BAND-LOW(WS-TX)
                   MOVE WS-TRIANGUL-BAND-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3560-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-ROUNDING.
           IF EURM-ROUNDING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-ROUNDING-AMT * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURM-ROUNDING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EURO YTD.
      *----------------------------------------------------------------*
       3590-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-YTD ROUNDED =
                   EURM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-EURO-YTD
           END-IF
           ADD EURM-EURO-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-FEE.
           IF EURM-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN EFFECTIVE DATE IS PAST
      * HYD -> JUL WHEN EFFECTIVE DATE IS PAST
      * JUL -> WSE WHEN EFFECTIVE DATE IS PAST
      * WSE -> YAU WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HYD' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HYD'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JUL' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JUL'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WSE' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WSE'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YAU' TO EURM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CURRENCY-CD
                   ADD EURM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3640-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   COMPUTE EURM-FEE-YTD ROUNDED = EURT-FEE-AMT *
                       EURM-RATE / 200
               WHEN EURT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO EURM-FEE-YTD
               WHEN EURT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO EURM-FEE-YTD
               WHEN OTHER
                   COMPUTE EURM-FEE-YTD ROUNDED = EURT-FEE-AMT *
                       EURM-RATE / 100
           END-EVALUATE
           IF EURM-FEE-YTD > 0 AND EURM-FEE-YTD < 2.00
               MOVE 2.00 TO EURM-FEE-YTD
           END-IF
           IF EURM-FEE-YTD > 100.00
               MOVE 100.00 TO EURM-FEE-YTD
           END-IF
           ADD EURM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO EURT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN LAST STMT DATE IS PAST
      * RV -> AD WHEN LAST STMT DATE IS PAST
      * AD -> RD WHEN LAST STMT DATE IS PAST
      * RD -> FX WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-FEE.
           IF EURM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-FEE-YTD * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-RESIDUAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROUNDING YTD.
      *----------------------------------------------------------------*
       3680-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-ROUNDING-YTD ROUNDED =
                   EURM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-ROUNDING-YTD
           END-IF
           ADD EURM-ROUNDING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E607 ADDR REQUIRED
      * E239 CONVERTED AMT NEGATIVE
      * E309 CONVERTED AMT OVER 50000
      * E023 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-ADDR = SPACES
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN EURT-CONVERTED-AMT < 0
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN EURT-CONVERTED-AMT > 50000.00
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN EURM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E023' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 5
               MOVE 'Y' TO EURM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL YTD BY CONV METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CONV-METHOD-CD
                   ADD EURM-NATIONAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-NATIONAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E063 MEMO REQUIRED
      * E743 CONVERTED YTD NEGATIVE
      * E309 CONVERTED YTD OVER 250000
      * E846 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-MEMO = SPACES
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-YTD < 0
                   MOVE 'E743' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-YTD > 250000.00
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN EURM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-OPEN.
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
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CONV METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-CONV-METHOD.
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
      * STATUS TRANSITIONS FOR REGION CD:
      * DTL -> HYD WHEN LAST STMT DATE IS PAST
      * HYD -> JUL WHEN LAST STMT DATE IS PAST
      * JUL -> WSE WHEN LAST STMT DATE IS PAST
      * WSE -> YAU WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-REGION.
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
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO EURM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-OPEN.
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
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-EURO-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E629 CONTACT REQUIRED
      * E271 CONVERTED AMT NEGATIVE
      * E548 CONVERTED AMT OVER 25000
      * E739 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E629' TO WS-REJECT-CODE
               WHEN EURT-CONVERTED-AMT < 0
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN EURT-CONVERTED-AMT > 25000.00
                   MOVE 'E548' TO WS-REJECT-CODE
               WHEN EURM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURT-FEE-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-FEE-PCT
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
       3810-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 10
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RESIDUAL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3820-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EURO AMT.
      *----------------------------------------------------------------*
       3850-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-AMT ROUNDED =
                   EURM-TRIANGULAR-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-EURO-AMT
           END-IF
           ADD EURM-EURO-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN EFFECTIVE DATE IS PAST
      * SMF -> UEC WHEN EFFECTIVE DATE IS PAST
      * UEC -> VJA WHEN EFFECTIVE DATE IS PAST
      * VJA -> WPH WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN EFF DATE IS PAST
      * RV -> AD WHEN EFF DATE IS PAST
      * AD -> RD WHEN EFF DATE IS PAST
      * RD -> FX WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NATIONAL YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3880-LIMIT-NATIONAL.
           IF EURM-NATIONAL-YTD < 100.00
               MOVE 100.00 TO EURM-NATIONAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-NATIONAL-YTD > 10000.00
               MOVE 10000.00 TO EURM-NATIONAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-CONVERTED.
           IF EURM-CONVERTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-CONVERTED-AMT * EURM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-TRIANGULAR-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURM-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURM-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE EURT-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E485 REF NO REQUIRED
      * E738 CONVERTED YTD NEGATIVE
      * E238 CONVERTED YTD OVER 99999
      * E783 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-REF-NO = SPACES
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-YTD < 0
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN EURM-CONVERTED-YTD > 99999.00
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN EURM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E209 ADDR REQUIRED
      * E906 RESIDUAL YTD NEGATIVE
      * E239 RESIDUAL YTD OVER 1000000
      * E474 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-ADDR = SPACES
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-YTD < 0
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-YTD > 1000000.00
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN EURM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E474' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3940-FLAG-ROUNDING.
           IF EURM-ROUNDING-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-ROUNDING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE NATIONAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-NATIONAL-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-BAND-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-TRIANGUL-BAND-LOW(WS-TX)
                   MOVE WS-TRIANGUL-BAND-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3970-FLAG-NATIONAL.
           IF EURM-NATIONAL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-NATIONAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-FEE.
           IF EURM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-FEE-AMT * EURM-RATE / 100 / 360
               ADD WS-WORK-AMT TO EURM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESIDUAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3990-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURT-FEE-AMT ROUNDED =
                   EURM-EURO-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURT-FEE-AMT
           END-IF
           ADD EURT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-NEXT-REVIEW.
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
               WHEN WS-DAYS NOT > 30
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-ROUNDING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4020-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN EFF DATE IS PAST
      * JA -> LD WHEN EFF DATE IS PAST
      * LD -> MR WHEN EFF DATE IS PAST
      * MR -> VC WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EBM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GAU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GSU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VWP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XDF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN MATURITY DATE IS PAST
      * JA -> LD WHEN MATURITY DATE IS PAST
      * LD -> MR WHEN MATURITY DATE IS PAST
      * MR -> VC WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-YTD - (WS-WORK-AMT *
               4)
           COMPUTE EURM-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 3
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EURO FEE ON THE CONVERTED AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4080-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-EURO-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   COMPUTE EURM-EURO-AMT ROUNDED = EURM-CONVERTED-AMT *
                       EURM-PENALTY-PCT / 200
               WHEN EURT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO EURM-EURO-AMT
               WHEN EURT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO EURM-EURO-AMT
               WHEN OTHER
                   COMPUTE EURM-EURO-AMT ROUNDED = EURM-CONVERTED-AMT *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-EURO-AMT > 0 AND EURM-EURO-AMT < 1.00
               MOVE 1.00 TO EURM-EURO-AMT
           END-IF
           IF EURM-EURO-AMT > 25.00
               MOVE 25.00 TO EURM-EURO-AMT
           END-IF
           ADD EURM-EURO-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE EURO AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NATIONAL YTD.
      *----------------------------------------------------------------*
       4090-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-NATIONAL-YTD ROUNDED =
                   EURM-EURO-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-NATIONAL-YTD
           END-IF
           ADD EURM-NATIONAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-NOTICE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO EURT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT < 0.00
               MOVE 0.00 TO EURM-TRIANGULAR-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-AMT > 50000.00
               MOVE 50000.00 TO EURM-TRIANGULAR-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN MATURITY DATE IS PAST
      * EBM -> GAU WHEN MATURITY DATE IS PAST
      * GAU -> GSU WHEN MATURITY DATE IS PAST
      * GSU -> VWP WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-CHANNEL.
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
      * ROUNDING YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4130-FLAG-ROUNDING.
           IF EURM-ROUNDING-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-ROUNDING-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-EXCEPT.
           IF EURM-EXCEPT-CNT < 9999999
               ADD 1 TO EURM-EXCEPT-CNT
           END-IF
           IF EURM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4160-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-EFF.
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
      * PRORATE ROUNDING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NATIONAL YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-NATIONAL-YTD ROUNDED =
                   EURT-ROUNDING-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-NATIONAL-YTD
           END-IF
           ADD EURM-NATIONAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-LAST-STMT.
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
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL BAND TIER FOR THE ROUNDING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-BAND-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF EURM-ROUNDING-YTD NOT < WS-TRIANGUL-BAND-LOW(WS-TX)
                   MOVE WS-TRIANGUL-BAND-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * EW -> FJ WHEN EFF DATE IS PAST
      * FJ -> JC WHEN EFF DATE IS PAST
      * JC -> EW WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-CURRENCY.
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
      * PRORATE NATIONAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESIDUAL AMT.
      *----------------------------------------------------------------*
       4220-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-AMT ROUNDED =
                   EURT-NATIONAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-FEE.
           IF EURM-FEE-AMT < 10.00
               MOVE 10.00 TO EURM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-FEE-AMT > 50000.00
               MOVE 50000.00 TO EURM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-EXCEPT.
           IF EURM-EXCEPT-CNT < 9999999
               ADD 1 TO EURM-EXCEPT-CNT
           END-IF
           IF EURM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE EURO YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF EURM-EURO-YTD NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-EFF.
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
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-CONVERTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESIDUAL AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-RESIDUAL.
           IF EURM-RESIDUAL-AMT < 50.00
               MOVE 50.00 TO EURM-RESIDUAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-RESIDUAL-AMT > 1000.00
               MOVE 1000.00 TO EURM-RESIDUAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E122 CONTACT REQUIRED
      * E926 FEE AMT NEGATIVE
      * E257 FEE AMT OVER 25000
      * E605 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN EURM-FEE-AMT < 0
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN EURM-FEE-AMT > 25000.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN EURM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4300-FLAG-FEE.
           IF EURT-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURT-ROUNDING-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURT-ROUNDING-AMT - (WS-WORK-AMT * 3)
           COMPUTE EURM-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURT-FEE-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4330-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD < 100.00
               MOVE 100.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-YTD > 50000.00
               MOVE 50000.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4340-LIMIT-ROUNDING.
           IF EURM-ROUNDING-YTD < 25.00
               MOVE 25.00 TO EURM-ROUNDING-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-ROUNDING-YTD > 5000.00
               MOVE 5000.00 TO EURM-ROUNDING-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4350-AGE-POST.
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
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-RESIDUAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4360-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 5
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-NEXT-DUE.
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
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-CONVERTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * EW -> FJ WHEN NOTICE DATE IS PAST
      * FJ -> JC WHEN NOTICE DATE IS PAST
      * JC -> EW WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4380-SET-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EW' TO EURM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4390-ACCRUE-NATIONAL.
           IF EURM-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-NATIONAL-YTD * EURM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-EURO-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL BAND TIER FOR THE EURO AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4400-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-BAND-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF EURM-EURO-AMT NOT < WS-TRIANGUL-BAND-LOW(WS-TX)
                   MOVE WS-TRIANGUL-BAND-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E476 REJECT CD REQUIRED
      * E631 NATIONAL AMT NEGATIVE
      * E793 NATIONAL AMT OVER 25000
      * E998 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-REJECT-CD = SPACES
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN EURT-NATIONAL-AMT < 0
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN EURT-NATIONAL-AMT > 25000.00
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN EURM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4420-LIMIT-ROUNDING.
           IF EURM-ROUNDING-YTD < 25.00
               MOVE 25.00 TO EURM-ROUNDING-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-ROUNDING-YTD > 10000.00
               MOVE 10000.00 TO EURM-ROUNDING-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4430-DESCRIBE-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-ROUNDING.
           IF EURM-ROUNDING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-ROUNDING-YTD * EURM-RATE / 100 / 360
               ADD WS-WORK-AMT TO EURT-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EURM-NATIONAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EURM-NATIONAL-YTD - (WS-WORK-AMT * 4)
           COMPUTE EURT-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4460-ACCRUE-NATIONAL.
           IF EURT-NATIONAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-NATIONAL-AMT * EURM-RATE / 100 / 360
               ADD WS-WORK-AMT TO EURM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4470-FLAG-CONVERTED.
           IF EURM-CONVERTED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-CONVERTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESIDUAL AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4480-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-REGION-CD
                   ADD EURM-RESIDUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-RESIDUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EURO AMT.
      *----------------------------------------------------------------*
       4490-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-AMT ROUNDED =
                   EURT-CONVERTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-EURO-AMT
           END-IF
           ADD EURM-EURO-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESIDUAL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4500-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE CONVERTED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4510-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EURT-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURT-NATIONAL-AMT ROUNDED =
                       EURT-CONVERTED-AMT * EURM-RATE / 200
               WHEN EURM-STATUS-CD = 'B'
                   MOVE 10.00 TO EURT-NATIONAL-AMT
               WHEN EURM-STATUS-CD = 'F'
                   MOVE 10.00 TO EURT-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EURT-NATIONAL-AMT ROUNDED =
                       EURT-CONVERTED-AMT * EURM-RATE / 100
           END-EVALUATE
           IF EURT-NATIONAL-AMT > 0 AND EURT-NATIONAL-AMT < 1.00
               MOVE 1.00 TO EURT-NATIONAL-AMT
           END-IF
           IF EURT-NATIONAL-AMT > 150.00
               MOVE 150.00 TO EURT-NATIONAL-AMT
           END-IF
           ADD EURT-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONVERTE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONVERTE-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-CONVERTE-GRID-LOW(WS-TX)
                   MOVE WS-CONVERTE-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4530-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   EURM-CONVERTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE TRIANGUL TIER TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4540-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TRIANGUL-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF EURM-CONVERTED-AMT NOT < WS-TRIANGUL-TIER-LOW(WS-TX)
                   MOVE WS-TRIANGUL-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E064 MEMO REQUIRED
      * E525 NATIONAL AMT NEGATIVE
      * E619 NATIONAL AMT OVER 10000
      * E283 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4550-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-MEMO = SPACES
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN EURT-NATIONAL-AMT < 0
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN EURT-NATIONAL-AMT > 10000.00
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN EURT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4560-DESCRIBE-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4570-AGE-LAST-MAINT.
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
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-ROUNDING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4580-ACCRUE-FEE.
           IF EURM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-FEE-AMT * EURM-RATE / 100 / 360
               ADD WS-WORK-AMT TO EURT-ROUNDING-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4590-CHECK-POST.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE TRIANGULAR AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4600-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EURM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   COMPUTE EURM-FEE-YTD ROUNDED = EURM-TRIANGULAR-AMT *
                       EURM-FEE-PCT / 200
               WHEN EURM-REGION-CD = 'DTL'
                   MOVE 3.00 TO EURM-FEE-YTD
               WHEN EURM-REGION-CD = 'HYD'
                   MOVE 20.00 TO EURM-FEE-YTD
               WHEN OTHER
                   COMPUTE EURM-FEE-YTD ROUNDED = EURM-TRIANGULAR-AMT *
                       EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-FEE-YTD > 0 AND EURM-FEE-YTD < 2.00
               MOVE 2.00 TO EURM-FEE-YTD
           END-IF
           IF EURM-FEE-YTD > 150.00
               MOVE 150.00 TO EURM-FEE-YTD
           END-IF
           ADD EURM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESIDUAL AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4610-ACCRUE-RESIDUAL.
           IF EURM-RESIDUAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-RESIDUAL-AMT * EURM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4620-DESCRIBE-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
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
                   MOVE EUVAL-RESIDUAL-YTD
                     TO EURM-RESIDUAL-YTD
                   MOVE EUVAL-OVERRIDE-FLAG
                     TO EURM-OVERRIDE-FLAG
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

      *----------------------------------------------------------------*
      * PASS THE EUR FIELDS TO EUELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-EUELG.
           INITIALIZE EUELG-PARMS
           MOVE EURM-ROUNDING-AMT
             TO EUELG-ROUNDING-AMT
           MOVE EURM-FEE-AMT
             TO EUELG-FEE-AMT
           MOVE EURM-FEE-YTD
             TO EUELG-FEE-YTD
           MOVE EURM-CONVERTED-AMT
             TO EUELG-CONVERTED-AMT
           MOVE EURM-TRIANGULAR-AMT
             TO EUELG-TRIANGULAR-AMT
           MOVE EURM-NATIONAL-YTD
             TO EUELG-NATIONAL-YTD
           MOVE EURM-OPEN-DATE
             TO EUELG-OPEN-DATE
           MOVE EURM-LAST-PAY-DATE
             TO EUELG-LAST-PAY-DATE
           MOVE EURM-NOTICE-DATE
             TO EUELG-NOTICE-DATE
           MOVE EURM-NEXT-REVIEW-DATE
             TO EUELG-NEXT-REVIEW-DATE
           MOVE EURM-CHANNEL-CD
             TO EUELG-CHANNEL-CD
           MOVE EURM-STATUS-CD
             TO EUELG-STATUS-CD
           MOVE EURM-CURRENCY-CD
             TO EUELG-CURRENCY-CD
           MOVE EURM-TYPE-CD
             TO EUELG-TYPE-CD
           MOVE EURM-PREFERRED-FLAG
             TO EUELG-PREFERRED-FLAG
           MOVE EURM-REVIEW-FLAG
             TO EUELG-REVIEW-FLAG
           MOVE EURM-EXEMPT-FLAG
             TO EUELG-EXEMPT-FLAG
           MOVE EURM-CYCLE-CNT
             TO EUELG-CYCLE-CNT
           MOVE EURM-ITEM-CNT
             TO EUELG-ITEM-CNT
           MOVE EURM-RATE
             TO EUELG-RATE
           MOVE EURM-FEE-PCT
             TO EUELG-FEE-PCT
           MOVE EURM-CUST-ID
             TO EUELG-CUST-ID
           MOVE EURM-ADDR
             TO EUELG-ADDR
           CALL 'EUELG' USING EUELG-PARMS
           EVALUATE EUELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE EUELG-FEE-AMT
                     TO EURM-FEE-AMT
                   MOVE EUELG-FEE-YTD
                     TO EURM-FEE-YTD
                   MOVE EUELG-CONVERTED-AMT
                     TO EURM-CONVERTED-AMT
                   MOVE EUELG-NATIONAL-YTD
                     TO EURM-NATIONAL-YTD
                   MOVE EUELG-PREFERRED-FLAG
                     TO EURM-PREFERRED-FLAG
                   MOVE EUELG-REVIEW-FLAG
                     TO EURM-REVIEW-FLAG
                   MOVE EUELG-EXEMPT-FLAG
                     TO EURM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-EUELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE EUPRC-EURO-AMT
                     TO EURM-EURO-AMT
                   MOVE EUPRC-FEE-AMT
                     TO EURM-FEE-AMT
                   MOVE EUPRC-CONVERTED-AMT
                     TO EURM-CONVERTED-AMT
                   MOVE EUPRC-REVIEW-FLAG
                     TO EURM-REVIEW-FLAG
                   MOVE EUPRC-AUTO-RENEW-FLAG
                     TO EURM-AUTO-RENEW-FLAG
                   MOVE EUPRC-OVERRIDE-FLAG
                     TO EURM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-EUPRC' TO ABND-PARAGRAPH
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
           DISPLAY 'EUREDIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'EUREDIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
