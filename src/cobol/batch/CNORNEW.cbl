      *================================================================*
      * PROGRAM  : CNORNEW                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COIN AND CURRENCY - RENEWAL. READS THE CCO          *
      *            TRANSACTIONS, UPDATES THE CCO MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BRANCH COIN AND CURRENCY ORDERS *
      *            AND ARMORED CARRIER SHIPMENT                        *
      * INPUT    : CCOMST (KSDS), CCOTRN, BANKCTL                      *
      * OUTPUT   : CCOOUT, CNORNRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CNDTE, CNPEN, CNFEE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-03-26 MKL  ORIGINAL                                       *
      * 1988-05-25 RJT  FEE EXCEPTIONS TO OPS QUEUE PRB6531            *
      * 1991-05-19 DPS  CURRENCY TABLE EXPANDED CHG8032                *
      * 1992-02-14 SKN  VARIANCE RERUN PROTECTION INC6759              *
      * 1992-10-20 EWS  COIN CALCULATION CORRECTED PRB5574             *
      * 1994-07-16 JLM  COIN CALCULATION CORRECTED PRB2321             *
      * 1995-10-26 GAF  CURRENCY EXCEPTIONS TO OPS QUEUE CHG1125       *
      * 1996-01-22 GAF  STRAP CUTOFF MOVED CHG7652                     *
      * 1999-08-01 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-04 CRP  AUDIT FINDING - SHIPMENT CONTROL TOTALS CHG0490*
      * 2002-11-28 MKL  CURRENCY FEE SCHEDULE UPDATE INC0933           *
      * 2002-07-06 RJT  PERFORMANCE - FEE READ AHEAD CHG3314           *
      * 2003-11-20 EWS  PERFORMANCE - COIN READ AHEAD CHG5726          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNORNEW.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "CNORNRPT"
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
      * WS-SHIPMENT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPMENT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000273000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000212500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000155000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000223500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000636500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000097500'.
       01  WS-SHIPMENT-TIER-TBL REDEFINES WS-SHIPMENT-TIER-VALUES.
           05  WS-SHIPMENT-TIER-ROW OCCURS 6 TIMES.
               10  WS-SHIPMENT-TIER-LOW    PIC 9(09)V99.
               10  WS-SHIPMENT-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VARIANCE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VARIANCE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000656000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000443500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000848000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000015000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000121000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030250000000662000'.
       01  WS-VARIANCE-GRID-TBL REDEFINES WS-VARIANCE-GRID-VALUES.
           05  WS-VARIANCE-GRID-ROW OCCURS 6 TIMES.
               10  WS-VARIANCE-GRID-LOW    PIC 9(09)V99.
               10  WS-VARIANCE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CURRENCY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CURRENCY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000846500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000840000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000786500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000355000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000323500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000108000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000558500'.
       01  WS-CURRENCY-TIER-TBL REDEFINES WS-CURRENCY-TIER-VALUES.
           05  WS-CURRENCY-TIER-ROW OCCURS 8 TIMES.
               10  WS-CURRENCY-TIER-LOW    PIC 9(09)V99.
               10  WS-CURRENCY-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000394000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000548500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000524000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000180000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004100000000556500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000542500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009100000000672000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000355500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024100000000232500'.
       01  WS-FEE-BAND-TBL REDEFINES WS-FEE-BAND-VALUES.
           05  WS-FEE-BAND-ROW OCCURS 9 TIMES.
               10  WS-FEE-BAND-LOW         PIC 9(09)V99.
               10  WS-FEE-BAND-RATE        PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CNORNEW  COIN AND CURRENCY'.
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
       COPY CNDTEP.
       COPY CNPENP.
       COPY CNFEEP.

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
               PERFORM 3000-EDIT-REF-NO
               PERFORM 3010-COUNT-RETRY
               PERFORM 3020-SET-CARRIER
               PERFORM 3030-EDIT-MEMO
               PERFORM 3040-SELECT-FEE-PCT
               PERFORM 3050-LIMIT-COIN
               PERFORM 3060-EDIT-NAME
               PERFORM 3070-PRORATE-VARIANCE
               PERFORM 3080-SPLIT-CURRENCY
               PERFORM 3090-EDIT-REF-NO
               PERFORM 3100-DESCRIBE-STATUS
               PERFORM 3110-CHECK-POST
               PERFORM 3120-FLAG-SHIPMENT
               PERFORM 3130-CHECK-MATURITY
               PERFORM 3140-ACCRUE-COIN
               PERFORM 3150-PRORATE-COIN
               PERFORM 3160-COUNT-CYCLE
               PERFORM 3170-CHECK-EXPIRY
               PERFORM 3180-ASSESS-AUTO-RENEW
               PERFORM 3190-DESCRIBE-REASON
               PERFORM 3200-ASSESS-AUTO-RENEW
               PERFORM 3210-EDIT-NAME
               PERFORM 3220-ASSESS-STOP
               PERFORM 3230-SELECT-PENALTY-PCT
               PERFORM 3240-ASSESS-HOLD
               PERFORM 3250-EDIT-DESC
               PERFORM 3260-ACCUM-DENOM
               PERFORM 3270-ACCUM-TRAN
               PERFORM 3280-FLAG-VARIANCE
               PERFORM 3290-EDIT-CONTACT
               PERFORM 3300-CHECK-POST
               PERFORM 3310-SELECT-PENALTY-PCT
               PERFORM 3320-SELECT-PENALTY-PCT
               PERFORM 3330-ACCRUE-COIN
               PERFORM 3340-EDIT-CONTACT
               PERFORM 3350-ASSESS-REVERSAL
               PERFORM 3360-PRORATE-COIN
               PERFORM 3370-LIMIT-ITEM
               PERFORM 3380-DESCRIBE-DENOM
               PERFORM 3390-ASSESS-NOTICE-SENT
               PERFORM 3400-SPLIT-SHIPMENT
               PERFORM 3410-SPLIT-VARIANCE
               PERFORM 3420-SET-SOURCE
               PERFORM 3430-ACCUM-CARRIER
               PERFORM 3440-COUNT-RETRY
               PERFORM 3450-EDIT-CUST-ID
               PERFORM 3460-DESCRIBE-CARRIER
               PERFORM 3470-ACCRUE-SHIPMENT
               PERFORM 3480-SPLIT-STRAP
               PERFORM 3490-FLAG-ITEM
               PERFORM 3500-ASSESS-OVERRIDE
               PERFORM 3510-COUNT-CYCLE
               PERFORM 3520-SPLIT-STRAP
               PERFORM 3530-COUNT-SEQ
               PERFORM 3540-ASSESS-EXEMPT
               PERFORM 3550-ACCUM-DENOM
               PERFORM 3560-COUNT-CYCLE
               PERFORM 3570-SET-CHANNEL
               PERFORM 3580-FLAG-FEE
               PERFORM 3590-SPLIT-SHIPMENT
               PERFORM 3600-FLAG-STRAP
               PERFORM 3610-DESCRIBE-CHANNEL
               PERFORM 3620-LIMIT-COIN
               PERFORM 3630-ASSESS-OVERRIDE
               PERFORM 3640-PRORATE-FEE
               PERFORM 3650-SET-CARRIER
               PERFORM 3660-LIMIT-STRAP
               PERFORM 3670-ACCRUE-PENALTY-PCT
               PERFORM 3680-ASSESS-STOP
               PERFORM 3690-ACCUM-DENOM
               PERFORM 3700-COUNT-ITEM
               PERFORM 3710-AGE-EFF
               PERFORM 3720-SET-REASON
               PERFORM 3730-PRORATE-VARIANCE
               PERFORM 3740-CHECK-NEXT-DUE
               PERFORM 3750-SELECT-PENALTY-PCT
               PERFORM 3760-COUNT-CYCLE
               PERFORM 3770-ACCUM-CHANNEL
               PERFORM 3780-EDIT-NAME
               PERFORM 3790-PRORATE-FEE
               PERFORM 3800-EDIT-OPERATOR
               PERFORM 3810-ACCRUE-CURRENCY
               PERFORM 3820-LIMIT-VARIANCE
               PERFORM 3830-DESCRIBE-CARRIER
               PERFORM 3840-SPLIT-COIN
               PERFORM 3850-ASSESS-OVERRIDE
               PERFORM 3860-AGE-NOTICE
               PERFORM 3870-LIMIT-ITEM
               PERFORM 3880-FLAG-COIN
               PERFORM 3890-ASSESS-EXEMPT
               PERFORM 3900-ACCUM-DENOM
               PERFORM 3910-EDIT-NAME
               PERFORM 3920-EDIT-CUST-ID
               PERFORM 3930-AGE-LAST-ACT
               PERFORM 3940-CHECK-EFF
               PERFORM 3950-ASSESS-REVERSAL
               PERFORM 3960-COUNT-SEQ
               PERFORM 3970-CHECK-NEXT-DUE
               PERFORM 3980-EDIT-CONTACT
               PERFORM 3990-PRORATE-CURRENCY
               PERFORM 4000-LIMIT-STRAP
               PERFORM 4010-EDIT-REJECT
               PERFORM 4020-EDIT-CUST-ID
               PERFORM 4030-SPLIT-SHIPMENT
               PERFORM 4040-DESCRIBE-STATUS
               PERFORM 4050-AGE-POST
               PERFORM 4060-SELECT-PENALTY-PCT
               PERFORM 4070-ACCUM-SOURCE
               PERFORM 4080-ASSESS-EXEMPT
               PERFORM 4090-EDIT-DESC
               PERFORM 4100-AGE-OPEN
               PERFORM 4110-LIMIT-SHIPMENT
               PERFORM 4120-PRORATE-SHIPMENT
               PERFORM 4130-AGE-EXPIRY
               PERFORM 4140-EDIT-ADDR
               PERFORM 4150-SELECT-RATE
               PERFORM 4160-CHECK-EXPIRY
               PERFORM 4170-COUNT-CYCLE
               PERFORM 4180-SPLIT-COIN
               PERFORM 4190-FLAG-COIN
               PERFORM 4200-COUNT-SEQ
               PERFORM 4210-ACCRUE-VARIANCE
               PERFORM 4220-SPLIT-ITEM
               PERFORM 4230-SET-STATUS
               PERFORM 4240-SELECT-PENALTY-PCT
               PERFORM 4250-LIMIT-COIN
               PERFORM 4260-ACCUM-REASON
               PERFORM 4270-CHECK-NOTICE
               PERFORM 4280-ACCUM-STATUS
               PERFORM 4290-DESCRIBE-TRAN
               PERFORM 4300-EDIT-REF-NO
               PERFORM 4310-ACCUM-CARRIER
               PERFORM 4320-CHECK-OPEN
               PERFORM 4330-SET-TRAN
               PERFORM 4340-LIMIT-COIN
               PERFORM 4350-SELECT-RATE
               PERFORM 7000-CALL-CNDTE
               PERFORM 7010-CALL-CNPEN
               PERFORM 7030-CALL-CNFEE
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E714 REF NO REQUIRED
      * E657 FEE YTD NEGATIVE
      * E973 FEE YTD OVER 25000
      * E544 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-REF-NO = SPACES
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD < 0
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD > 25000.00
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN POST DATE IS PAST
      * L -> M WHEN POST DATE IS PAST
      * M -> X WHEN POST DATE IS PAST
      * X -> J WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E532 MEMO REQUIRED
      * E372 CURRENCY YTD NEGATIVE
      * E547 CURRENCY YTD OVER 50000
      * E060 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3030-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-MEMO = SPACES
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD < 0
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD > 50000.00
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE VARIANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CCOM-VARIANCE-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-COIN.
           IF CCOT-COIN-AMT < 0.00
               MOVE 0.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 10000.00
               MOVE 10000.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E469 NAME REQUIRED
      * E359 WORK AMT 2 NEGATIVE
      * E107 WORK AMT 2 OVER 50000
      * E080 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CURRENCY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VARIANCE YTD.
      *----------------------------------------------------------------*
       3070-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-YTD ROUNDED =
                   CCOM-CURRENCY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-AMT - (WS-WORK-AMT * 3)
           COMPUTE CCOM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E897 REF NO REQUIRED
      * E378 CURRENCY YTD NEGATIVE
      * E626 CURRENCY YTD OVER 10000
      * E373 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-REF-NO = SPACES
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD < 0
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD > 10000.00
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN CCOM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3120-FLAG-SHIPMENT.
           IF CCOM-SHIPMENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-MATURITY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-COIN.
           IF CCOT-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-COIN-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOT-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COIN AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-COIN-AMT ROUNDED =
                   CCOM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-COIN-AMT
           END-IF
           ADD CCOT-COIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT YTD FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3180-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-YTD ROUNDED = CCOM-SHIPMENT-AMT
                       * CCOM-FEE-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 12.00 TO CCOM-SHIPMENT-YTD
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 7.00 TO CCOM-SHIPMENT-YTD
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-YTD ROUNDED = CCOM-SHIPMENT-AMT
                       * CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-YTD > 0 AND CCOM-SHIPMENT-YTD < 10.00
               MOVE 10.00 TO CCOM-SHIPMENT-YTD
           END-IF
           IF CCOM-SHIPMENT-YTD > 100.00
               MOVE 100.00 TO CCOM-SHIPMENT-YTD
           END-IF
           ADD CCOM-SHIPMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE COIN AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3200-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CCOM-COIN-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CCOM-COIN-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E416 NAME REQUIRED
      * E099 SHIPMENT AMT NEGATIVE
      * E511 SHIPMENT AMT OVER 250000
      * E438 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-AMT < 0
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-AMT > 250000.00
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E438' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE VARIANCE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3220-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOT-VARIANCE-AMT
                       * CCOM-RATE / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 7.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 12.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOT-VARIANCE-AMT
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 1.00
               MOVE 1.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 50.00
               MOVE 50.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE GRID TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-GRID-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOT-COIN-AMT NOT < WS-VARIANCE-GRID-LOW(WS-TX)
                   MOVE WS-VARIANCE-GRID-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE YTD FEE ON THE COIN AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3240-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-COIN-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-REASON-CD = 'JL'
                   MOVE 10.00 TO CCOM-VARIANCE-YTD
               WHEN CCOM-REASON-CD = 'LW'
                   MOVE 3.00 TO CCOM-VARIANCE-YTD
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-COIN-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-VARIANCE-YTD > 0 AND CCOM-VARIANCE-YTD < 1.00
               MOVE 1.00 TO CCOM-VARIANCE-YTD
           END-IF
           IF CCOM-VARIANCE-YTD > 35.00
               MOVE 35.00 TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E635 DESC REQUIRED
      * E889 FEE YTD NEGATIVE
      * E048 FEE YTD OVER 99999
      * E500 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-DESC = SPACES
                   MOVE 'E635' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD < 0
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD > 99999.00
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN CCOT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-TRAN.
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
      * VARIANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3280-FLAG-VARIANCE.
           IF CCOT-VARIANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOT-VARIANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E261 CONTACT REQUIRED
      * E897 STRAP AMT NEGATIVE
      * E882 STRAP AMT OVER 99999
      * E839 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3290-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-AMT < 0
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-AMT > 99999.00
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN CCOM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-POST.
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
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT TIER TIER FOR THE CURRENCY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-TIER-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOM-CURRENCY-AMT NOT < WS-SHIPMENT-TIER-LOW(WS-TX)
                   MOVE WS-SHIPMENT-TIER-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-GRID-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOM-FEE-YTD NOT < WS-VARIANCE-GRID-LOW(WS-TX)
                   MOVE WS-VARIANCE-GRID-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-COIN.
           IF CCOT-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-COIN-AMT * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E333 CONTACT REQUIRED
      * E700 STRAP AMT NEGATIVE
      * E433 STRAP AMT OVER 99999
      * E967 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN CCOT-STRAP-AMT < 0
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN CCOT-STRAP-AMT > 99999.00
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN CCOM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E967' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT YTD FEE ON THE STRAP AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3350-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-YTD ROUNDED = CCOM-STRAP-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 12.00 TO CCOM-SHIPMENT-YTD
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 20.00 TO CCOM-SHIPMENT-YTD
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-YTD ROUNDED = CCOM-STRAP-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-YTD > 0 AND CCOM-SHIPMENT-YTD < 2.00
               MOVE 2.00 TO CCOM-SHIPMENT-YTD
           END-IF
           IF CCOM-SHIPMENT-YTD > 35.00
               MOVE 35.00 TO CCOM-SHIPMENT-YTD
           END-IF
           ADD CCOM-SHIPMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COIN AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-COIN-AMT ROUNDED =
                   CCOM-COIN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOT-COIN-AMT
           END-IF
           ADD CCOT-COIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE STRAP AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3390-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOM-STRAP-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOM-STRAP-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOT-SHIPMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CCOT-SHIPMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE CCOM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOT-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CFR -> DTX WHEN OPEN DATE IS PAST
      * DTX -> EEA WHEN OPEN DATE IS PAST
      * EEA -> EUA WHEN OPEN DATE IS PAST
      * EUA -> TPB WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-SOURCE.
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
      * ACCUMULATE STRAP YTD BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 12
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E090 CUST ID REQUIRED
      * E217 VARIANCE YTD NEGATIVE
      * E452 VARIANCE YTD OVER 250000
      * E798 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-YTD < 0
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-YTD > 250000.00
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN CCOM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-VARIANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOM-STRAP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-STRAP-YTD - (WS-WORK-AMT * 3)
           COMPUTE CCOT-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3490-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE COIN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3500-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CCOM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOM-FEE-AMT ROUNDED = CCOM-COIN-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-REASON-CD = 'JL'
                   MOVE 5.00 TO CCOM-FEE-AMT
               WHEN CCOM-REASON-CD = 'LW'
                   MOVE 10.00 TO CCOM-FEE-AMT
               WHEN OTHER
                   COMPUTE CCOM-FEE-AMT ROUNDED = CCOM-COIN-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-FEE-AMT > 0 AND CCOM-FEE-AMT < 2.00
               MOVE 2.00 TO CCOM-FEE-AMT
           END-IF
           IF CCOM-FEE-AMT > 50.00
               MOVE 50.00 TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOT-STRAP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOT-STRAP-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOM-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3530-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 12
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3540-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOT-SHIPMENT-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO CCOM-COIN-YTD
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOT-SHIPMENT-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 2.00
               MOVE 2.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 50.00
               MOVE 50.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3580-FLAG-FEE.
           IF CCOM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOM-SHIPMENT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-SHIPMENT-YTD - (WS-WORK-AMT * 4)
           COMPUTE CCOM-CURRENCY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3600-FLAG-STRAP.
           IF CCOM-STRAP-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-STRAP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COIN YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-COIN.
           IF CCOM-COIN-YTD < 100.00
               MOVE 100.00 TO CCOM-COIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-COIN-YTD > 50000.00
               MOVE 50000.00 TO CCOM-COIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE CURRENCY AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3630-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOM-CURRENCY-AMT *
                       CCOM-RATE / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 12.00 TO CCOM-COIN-YTD
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 3.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOM-CURRENCY-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 5.00
               MOVE 5.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 50.00
               MOVE 50.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE COIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-YTD ROUNDED =
                   CCOM-COIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-FEE-YTD
           END-IF
           ADD CCOM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> X WHEN NEXT REVIEW DATE IS PAST
      * X -> J WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STRAP YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3660-LIMIT-STRAP.
           IF CCOM-STRAP-YTD < 0.00
               MOVE 0.00 TO CCOM-STRAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-YTD > 2500.00
               MOVE 2500.00 TO CCOM-STRAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3680-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOM-SHIPMENT-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOM-SHIPMENT-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 5
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-EFF.
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
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN EFF DATE IS PAST
      * LW -> PS WHEN EFF DATE IS PAST
      * PS -> JL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE GRID TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-GRID-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOT-COIN-AMT NOT < WS-VARIANCE-GRID-LOW(WS-TX)
                   MOVE WS-VARIANCE-GRID-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3760-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD CCOM-COIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-COIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E067 NAME REQUIRED
      * E511 STRAP YTD NEGATIVE
      * E496 STRAP YTD OVER 10000
      * E747 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-YTD < 0
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-YTD > 10000.00
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN CCOM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-AMT ROUNDED =
                   CCOM-SHIPMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E637 OPERATOR REQUIRED
      * E237 CURRENCY AMT NEGATIVE
      * E610 CURRENCY AMT OVER 99999
      * E373 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-OPERATOR = SPACES
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT < 0
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT > 99999.00
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN CCOM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CURRENCY AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-CURRENCY.
           IF CCOM-CURRENCY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-CURRENCY-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3820-LIMIT-VARIANCE.
           IF CCOM-VARIANCE-YTD < 10.00
               MOVE 10.00 TO CCOM-VARIANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-VARIANCE-YTD > 10000.00
               MOVE 10000.00 TO CCOM-VARIANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COIN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOM-COIN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-COIN-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN FEE ON THE VARIANCE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3850-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CCOT-COIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOT-COIN-AMT ROUNDED = CCOM-VARIANCE-YTD *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 10.00 TO CCOT-COIN-AMT
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 7.00 TO CCOT-COIN-AMT
               WHEN OTHER
                   COMPUTE CCOT-COIN-AMT ROUNDED = CCOM-VARIANCE-YTD *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOT-COIN-AMT > 0 AND CCOT-COIN-AMT < 10.00
               MOVE 10.00 TO CCOT-COIN-AMT
           END-IF
           IF CCOT-COIN-AMT > 150.00
               MOVE 150.00 TO CCOT-COIN-AMT
           END-IF
           ADD CCOT-COIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3880-FLAG-COIN.
           IF CCOT-COIN-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOT-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE YTD FEE ON THE VARIANCE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3890-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOT-VARIANCE-AMT
                       * CCOM-FEE-PCT / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 12.00 TO CCOM-VARIANCE-YTD
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 5.00 TO CCOM-VARIANCE-YTD
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOT-VARIANCE-AMT
                       * CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-VARIANCE-YTD > 0 AND CCOM-VARIANCE-YTD < 2.00
               MOVE 2.00 TO CCOM-VARIANCE-YTD
           END-IF
           IF CCOM-VARIANCE-YTD > 150.00
               MOVE 150.00 TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3900-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E808 NAME REQUIRED
      * E849 WORK AMT 2 NEGATIVE
      * E129 WORK AMT 2 OVER 5000
      * E626 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E129' TO WS-REJECT-CODE
               WHEN CCOM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E199 CUST ID REQUIRED
      * E708 CURRENCY AMT NEGATIVE
      * E928 CURRENCY AMT OVER 10000
      * E536 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT < 0
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT > 10000.00
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN CCOM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-LAST-ACT.
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
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3950-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-FEE-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 5.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 3.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-FEE-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 1.00
               MOVE 1.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 25.00
               MOVE 25.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3960-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 3
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-NEXT-DUE.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E602 CONTACT REQUIRED
      * E370 VARIANCE YTD NEGATIVE
      * E142 VARIANCE YTD OVER 250000
      * E219 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-YTD < 0
                   MOVE 'E370' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-YTD > 250000.00
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN CCOM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CURRENCY AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-CURRENCY-AMT ROUNDED =
                   CCOM-SHIPMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4000-LIMIT-STRAP.
           IF CCOT-STRAP-AMT < 50.00
               MOVE 50.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-STRAP-AMT > 1000.00
               MOVE 1000.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E354 REJECT CD REQUIRED
      * E091 CURRENCY YTD NEGATIVE
      * E722 CURRENCY YTD OVER 25000
      * E080 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-REJECT-CD = SPACES
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD < 0
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-YTD > 25000.00
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E768 CUST ID REQUIRED
      * E650 VARIANCE AMT NEGATIVE
      * E765 VARIANCE AMT OVER 5000
      * E875 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E768' TO WS-REJECT-CODE
               WHEN CCOT-VARIANCE-AMT < 0
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN CCOT-VARIANCE-AMT > 5000.00
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOM-SHIPMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-SHIPMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE CCOM-SHIPMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-POST.
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
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE SHIPMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CCOM-SHIPMENT-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-SOURCE-CD
                   ADD CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4080-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = WS-WORK-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-REASON-CD = 'JL'
                   MOVE 7.00 TO CCOM-COIN-YTD
               WHEN CCOM-REASON-CD = 'LW'
                   MOVE 12.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = WS-WORK-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 2.00
               MOVE 2.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 35.00
               MOVE 35.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E313 DESC REQUIRED
      * E805 CURRENCY AMT NEGATIVE
      * E022 CURRENCY AMT OVER 50000
      * E652 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-DESC = SPACES
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT < 0
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT > 50000.00
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN CCOM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4100-AGE-OPEN.
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
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-COIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-YTD < 10.00
               MOVE 10.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-YTD > 1000.00
               MOVE 1000.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CURRENCY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       4120-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-SHIPMENT-AMT ROUNDED =
                   CCOM-CURRENCY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E574 ADDR REQUIRED
      * E896 WORK AMT NEGATIVE
      * E068 WORK AMT OVER 99999
      * E736 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-ADDR = SPACES
                   MOVE 'E574' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT TIER TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-TIER-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOM-STRAP-YTD NOT < WS-SHIPMENT-TIER-LOW(WS-TX)
                   MOVE WS-SHIPMENT-TIER-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOM-COIN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-COIN-AMT - (WS-WORK-AMT * 3)
           COMPUTE CCOM-STRAP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4190-FLAG-COIN.
           IF CCOM-COIN-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-COIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 5
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4210-ACCRUE-VARIANCE.
           IF CCOT-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOM-STRAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * ADH -> FVV WHEN LAST PAY DATE IS PAST
      * FVV -> GCB WHEN LAST PAY DATE IS PAST
      * GCB -> UZP WHEN LAST PAY DATE IS PAST
      * UZP -> VXB WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FVV' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FVV'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GCB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GCB'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZP' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UZP'
                   IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VXB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHIPMENT TIER TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPMENT-TIER-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CCOM-COIN-AMT NOT < WS-SHIPMENT-TIER-LOW(WS-TX)
                   MOVE WS-SHIPMENT-TIER-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4250-LIMIT-COIN.
           IF CCOT-COIN-AMT < 10.00
               MOVE 10.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 50000.00
               MOVE 50000.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4270-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
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
      * ACCUMULATE VARIANCE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-STATUS-CD
                   ADD CCOT-VARIANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOT-VARIANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E543 REF NO REQUIRED
      * E412 VARIANCE AMT NEGATIVE
      * E516 VARIANCE AMT OVER 250000
      * E811 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-REF-NO = SPACES
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN CCOT-VARIANCE-AMT < 0
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN CCOT-VARIANCE-AMT > 250000.00
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN CCOM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E811' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP AMT BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOT-STRAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOT-STRAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4320-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> SH WHEN EFFECTIVE DATE IS PAST
      * SH -> RC WHEN EFFECTIVE DATE IS PAST
      * RC -> VR WHEN EFFECTIVE DATE IS PAST
      * VR -> AD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4340-LIMIT-COIN.
           IF CCOT-COIN-AMT < 25.00
               MOVE 25.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 2500.00
               MOVE 2500.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE SHIPMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CCOM-SHIPMENT-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-CNDTE.
           INITIALIZE CNDTE-PARMS
           MOVE CCOM-SHIPMENT-AMT
             TO CNDTE-SHIPMENT-AMT
           MOVE CCOM-FEE-AMT
             TO CNDTE-FEE-AMT
           MOVE CCOM-COIN-YTD
             TO CNDTE-COIN-YTD
           MOVE CCOM-FEE-YTD
             TO CNDTE-FEE-YTD
           MOVE CCOM-COIN-AMT
             TO CNDTE-COIN-AMT
           MOVE CCOM-CURRENCY-YTD
             TO CNDTE-CURRENCY-YTD
           MOVE CCOM-EFFECTIVE-DATE
             TO CNDTE-EFFECTIVE-DATE
           MOVE CCOM-LAST-ACT-DATE
             TO CNDTE-LAST-ACT-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNDTE-NEXT-REVIEW-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNDTE-MATURITY-DATE
           MOVE CCOM-CHANNEL-CD
             TO CNDTE-CHANNEL-CD
           MOVE CCOM-DENOM-CD
             TO CNDTE-DENOM-CD
           MOVE CCOM-STATUS-CD
             TO CNDTE-STATUS-CD
           MOVE CCOM-CARRIER-CD
             TO CNDTE-CARRIER-CD
           MOVE CCOM-NOTICE-SENT-FLAG
             TO CNDTE-NOTICE-SENT-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNDTE-HOLD-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNDTE-AUTO-RENEW-FLAG
           MOVE CCOM-ITEM-CNT
             TO CNDTE-ITEM-CNT
           MOVE CCOM-NOTICE-CNT
             TO CNDTE-NOTICE-CNT
           MOVE CCOM-PENALTY-PCT
             TO CNDTE-PENALTY-PCT
           MOVE CCOM-FEE-PCT
             TO CNDTE-FEE-PCT
           MOVE CCOM-CUST-ID
             TO CNDTE-CUST-ID
           MOVE CCOM-DESC
             TO CNDTE-DESC
           CALL 'CNDTE' USING CNDTE-PARMS
           EVALUATE CNDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNDTE-FEE-AMT
                     TO CCOM-FEE-AMT
                   MOVE CNDTE-CURRENCY-YTD
                     TO CCOM-CURRENCY-YTD
                   MOVE CNDTE-HOLD-FLAG
                     TO CCOM-HOLD-FLAG
                   MOVE CNDTE-AUTO-RENEW-FLAG
                     TO CCOM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-CNDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-CNPEN.
           INITIALIZE CNPEN-PARMS
           MOVE CCOM-SHIPMENT-AMT
             TO CNPEN-SHIPMENT-AMT
           MOVE CCOM-COIN-AMT
             TO CNPEN-COIN-AMT
           MOVE CCOM-STRAP-YTD
             TO CNPEN-STRAP-YTD
           MOVE CCOM-SHIPMENT-YTD
             TO CNPEN-SHIPMENT-YTD
           MOVE CCOM-CURRENCY-YTD
             TO CNPEN-CURRENCY-YTD
           MOVE CCOM-FEE-AMT
             TO CNPEN-FEE-AMT
           MOVE CCOM-NOTICE-DATE
             TO CNPEN-NOTICE-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNPEN-NEXT-REVIEW-DATE
           MOVE CCOM-LAST-PAY-DATE
             TO CNPEN-LAST-PAY-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNPEN-MATURITY-DATE
           MOVE CCOM-CARRIER-CD
             TO CNPEN-CARRIER-CD
           MOVE CCOM-REASON-CD
             TO CNPEN-REASON-CD
           MOVE CCOM-SOURCE-CD
             TO CNPEN-SOURCE-CD
           MOVE CCOM-CHANNEL-CD
             TO CNPEN-CHANNEL-CD
           MOVE CCOM-STOP-FLAG
             TO CNPEN-STOP-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNPEN-HOLD-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNPEN-AUTO-RENEW-FLAG
           MOVE CCOM-RETRY-CNT
             TO CNPEN-RETRY-CNT
           MOVE CCOM-ITEM-CNT
             TO CNPEN-ITEM-CNT
           MOVE CCOM-RATE
             TO CNPEN-RATE
           MOVE CCOM-PENALTY-PCT
             TO CNPEN-PENALTY-PCT
           MOVE CCOM-REF-NO
             TO CNPEN-REF-NO
           MOVE CCOM-CONTACT
             TO CNPEN-CONTACT
           CALL 'CNPEN' USING CNPEN-PARMS
           EVALUATE CNPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNPEN-STRAP-YTD
                     TO CCOM-STRAP-YTD
                   MOVE CNPEN-SHIPMENT-YTD
                     TO CCOM-SHIPMENT-YTD
                   MOVE CNPEN-CURRENCY-YTD
                     TO CCOM-CURRENCY-YTD
                   MOVE CNPEN-FEE-AMT
                     TO CCOM-FEE-AMT
                   MOVE CNPEN-STOP-FLAG
                     TO CCOM-STOP-FLAG
                   MOVE CNPEN-HOLD-FLAG
                     TO CCOM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CNPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-CNFEE.
           INITIALIZE CNFEE-PARMS
           MOVE CCOM-STRAP-YTD
             TO CNFEE-STRAP-YTD
           MOVE CCOM-CURRENCY-YTD
             TO CNFEE-CURRENCY-YTD
           MOVE CCOM-STRAP-AMT
             TO CNFEE-STRAP-AMT
           MOVE CCOM-FEE-AMT
             TO CNFEE-FEE-AMT
           MOVE CCOM-VARIANCE-YTD
             TO CNFEE-VARIANCE-YTD
           MOVE CCOM-COIN-YTD
             TO CNFEE-COIN-YTD
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNFEE-NEXT-REVIEW-DATE
           MOVE CCOM-LAST-PAY-DATE
             TO CNFEE-LAST-PAY-DATE
           MOVE CCOM-NOTICE-DATE
             TO CNFEE-NOTICE-DATE
           MOVE CCOM-EFFECTIVE-DATE
             TO CNFEE-EFFECTIVE-DATE
           MOVE CCOM-REASON-CD
             TO CNFEE-REASON-CD
           MOVE CCOM-SOURCE-CD
             TO CNFEE-SOURCE-CD
           MOVE CCOM-DENOM-CD
             TO CNFEE-DENOM-CD
           MOVE CCOM-CARRIER-CD
             TO CNFEE-CARRIER-CD
           MOVE CCOM-EXEMPT-FLAG
             TO CNFEE-EXEMPT-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNFEE-AUTO-RENEW-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNFEE-HOLD-FLAG
           MOVE CCOM-CYCLE-CNT
             TO CNFEE-CYCLE-CNT
           MOVE CCOM-NOTICE-CNT
             TO CNFEE-NOTICE-CNT
           MOVE CCOM-RATE
             TO CNFEE-RATE
           MOVE CCOM-FEE-PCT
             TO CNFEE-FEE-PCT
           MOVE CCOM-DESC
             TO CNFEE-DESC
           MOVE CCOM-REF-NO
             TO CNFEE-REF-NO
           CALL 'CNFEE' USING CNFEE-PARMS
           EVALUATE CNFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNFEE-FEE-AMT
                     TO CCOM-FEE-AMT
                   MOVE CNFEE-VARIANCE-YTD
                     TO CCOM-VARIANCE-YTD
                   MOVE CNFEE-COIN-YTD
                     TO CCOM-COIN-YTD
                   MOVE CNFEE-HOLD-FLAG
                     TO CCOM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CNFEE' TO ABND-PARAGRAPH
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
           DISPLAY 'CNORNEW READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CNORNEW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
