      *================================================================*
      * PROGRAM  : AULPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DEALER AUTO FINANCE - PURGE AND ARCHIVE. READS THE  *
      *            ADL TRANSACTIONS, UPDATES THE ADL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. INDIRECT AUTO DEALER   *
      *            CONTRACTS, RESERVE AND CHARGEBACK                   *
      * INPUT    : ADLMST (KSDS), ADLTRN, BANKCTL                      *
      * OUTPUT   : ADLOUT, AULPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, AUDTE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-09-03 EWS  ORIGINAL                                       *
      * 1998-02-23 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-20 PQR  GAP NOTICE TEXT PER LEGAL PRB2786              *
      * 1998-06-20 BHS  GAP NOTICE TEXT PER LEGAL PRB6843              *
      * 2001-12-26 CRP  AUDIT FINDING - GAP CONTROL TOTALS REQ6620     *
      * 2003-08-26 BHS  REBATE CALCULATION CORRECTED PRB9881           *
      * 2003-06-27 BHS  PAYMENT FEE SCHEDULE UPDATE REQ3047            *
      * 2006-08-18 JLM  CHARGEBK CALCULATION CORRECTED CHG9473         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULPURG.
       AUTHOR. M K LOWE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ADLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ADLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ADLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ADLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "AULPURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ADLMST.
       FD  TRAN-FILE.
       COPY ADLTRN.
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
                                           VALUE 15.000000.
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
      * WS-GAP-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GAP-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000114000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000643000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000855500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000737500'.
       01  WS-GAP-GRID-TBL REDEFINES WS-GAP-GRID-VALUES.
           05  WS-GAP-GRID-ROW OCCURS 4 TIMES.
               10  WS-GAP-GRID-LOW         PIC 9(09)V99.
               10  WS-GAP-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GAP-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GAP-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000543500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000154000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000343500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000556000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000336000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000718500'.
       01  WS-GAP-BAND-TBL REDEFINES WS-GAP-BAND-VALUES.
           05  WS-GAP-BAND-ROW OCCURS 6 TIMES.
               10  WS-GAP-BAND-LOW         PIC 9(09)V99.
               10  WS-GAP-BAND-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESERVE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESERVE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000122000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000614000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000157000'.
       01  WS-RESERVE-BAND-TBL REDEFINES WS-RESERVE-BAND-VALUES.
           05  WS-RESERVE-BAND-ROW OCCURS 4 TIMES.
               10  WS-RESERVE-BAND-LOW     PIC 9(09)V99.
               10  WS-RESERVE-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  AULPURG  DEALER AUTO FINANCE'.
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
       COPY AUDTEP.

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
               MOVE 'ADLMST' TO ABND-FILE-NAME
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
           MOVE ADLT-KEY TO ADLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-EDIT-NAME
               PERFORM 3010-LIMIT-AMOUNT-FIN
               PERFORM 3020-EDIT-DESC
               PERFORM 3030-SELECT-RATE
               PERFORM 3040-COUNT-EXCEPT
               PERFORM 3050-AGE-EXPIRY
               PERFORM 3060-SELECT-FEE-PCT
               PERFORM 3070-AGE-NEXT-REVIEW
               PERFORM 3080-ASSESS-REVIEW
               PERFORM 3090-CHECK-NEXT-REVIEW
               PERFORM 3100-ASSESS-PREFERRED
               PERFORM 3110-ACCUM-STATUS
               PERFORM 3120-PRORATE-AMOUNT-FIN
               PERFORM 3130-FLAG-RESERVE
               PERFORM 3140-ACCUM-REASON
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-SPLIT-REBATE
               PERFORM 3170-ASSESS-OVERRIDE
               PERFORM 3180-LIMIT-REBATE
               PERFORM 3190-ACCUM-CYCLE
               PERFORM 3200-SPLIT-ITEM
               PERFORM 3210-PRORATE-GAP
               PERFORM 3220-LIMIT-REBATE
               PERFORM 3230-ACCUM-TYPE
               PERFORM 3240-FLAG-PAYMENT
               PERFORM 3250-SELECT-MARGIN
               PERFORM 3260-SET-CYCLE
               PERFORM 3270-PRORATE-AMOUNT-FIN
               PERFORM 3280-FLAG-RESERVE
               PERFORM 3290-SET-TYPE
               PERFORM 3300-ACCRUE-REBATE
               PERFORM 3310-PRORATE-RESERVE
               PERFORM 3320-LIMIT-PAYMENT
               PERFORM 3330-AGE-NOTICE
               PERFORM 3340-CHECK-NEXT-REVIEW
               PERFORM 3350-DESCRIBE-STATUS
               PERFORM 3360-EDIT-REJECT
               PERFORM 3370-ACCRUE-REBATE
               PERFORM 3380-AGE-NEXT-REVIEW
               PERFORM 3390-SELECT-FEE-PCT
               PERFORM 3400-SELECT-MARGIN
               PERFORM 3410-ACCUM-REASON
               PERFORM 3420-ACCUM-DEALER
               PERFORM 3430-SPLIT-RESERVE
               PERFORM 3440-CHECK-LAST-PAY
               PERFORM 3450-ACCUM-STATUS
               PERFORM 3460-SELECT-MARGIN
               PERFORM 3470-AGE-EXPIRY
               PERFORM 3480-DESCRIBE-REASON
               PERFORM 3490-PRORATE-GAP
               PERFORM 3500-SELECT-RATE
               PERFORM 3510-COUNT-EXCEPT
               PERFORM 3520-ACCUM-TIER
               PERFORM 3530-SPLIT-REBATE
               PERFORM 3540-SPLIT-REBATE
               PERFORM 3550-ASSESS-REVIEW
               PERFORM 3560-ACCRUE-PAYMENT
               PERFORM 3570-CHECK-LAST-MAINT
               PERFORM 3580-SPLIT-GAP
               PERFORM 3590-LIMIT-PAYMENT
               PERFORM 3600-SELECT-MARGIN
               PERFORM 3610-EDIT-MEMO
               PERFORM 3620-ASSESS-REVIEW
               PERFORM 3630-DESCRIBE-DEALER
               PERFORM 3640-SPLIT-PAYMENT
               PERFORM 3650-ACCRUE-CHARGEBK
               PERFORM 3660-EDIT-DESC
               PERFORM 3670-CHECK-LAST-STMT
               PERFORM 3680-ASSESS-EXEMPT
               PERFORM 3690-FLAG-RESERVE
               PERFORM 3700-SPLIT-CHARGEBK
               PERFORM 3710-ACCUM-CYCLE
               PERFORM 3720-ASSESS-REVIEW
               PERFORM 3730-FLAG-PAYMENT
               PERFORM 3740-PRORATE-PAYMENT
               PERFORM 3750-ACCUM-TIER
               PERFORM 3760-CHECK-POST
               PERFORM 3770-LIMIT-CHARGEBK
               PERFORM 3780-ACCUM-TRAN
               PERFORM 3790-ACCUM-DEALER
               PERFORM 3800-COUNT-SEQ
               PERFORM 3810-LIMIT-REBATE
               PERFORM 3820-AGE-NEXT-REVIEW
               PERFORM 3830-AGE-NEXT-REVIEW
               PERFORM 3840-COUNT-CYCLE
               PERFORM 3850-ACCRUE-PAYMENT
               PERFORM 3860-ACCRUE-RESERVE
               PERFORM 3870-ACCUM-STATUS
               PERFORM 3880-ASSESS-EXEMPT
               PERFORM 3890-AGE-LAST-MAINT
               PERFORM 3900-SPLIT-PAYMENT
               PERFORM 3910-SET-TYPE
               PERFORM 3920-PRORATE-RESERVE
               PERFORM 3930-CHECK-EFF
               PERFORM 3940-CHECK-LAST-STMT
               PERFORM 3950-ACCRUE-PAYMENT
               PERFORM 3960-COUNT-CYCLE
               PERFORM 3970-DESCRIBE-TRAN
               PERFORM 3980-SET-TIER
               PERFORM 3990-ASSESS-PREFERRED
               PERFORM 4000-SPLIT-REBATE
               PERFORM 4010-AGE-LAST-ACT
               PERFORM 4020-FLAG-ITEM
               PERFORM 4030-PRORATE-RESERVE
               PERFORM 4040-ACCRUE-REBATE
               PERFORM 4050-EDIT-DESC
               PERFORM 4060-CHECK-LAST-ACT
               PERFORM 4070-DESCRIBE-STATUS
               PERFORM 4080-ASSESS-REVIEW
               PERFORM 4090-SET-TRAN
               PERFORM 4100-EDIT-MEMO
               PERFORM 4110-FLAG-REBATE
               PERFORM 4120-EDIT-CONTACT
               PERFORM 4130-COUNT-PAYMENT
               PERFORM 4140-AGE-EXPIRY
               PERFORM 4150-ACCRUE-FEE-PCT
               PERFORM 4160-SET-REASON
               PERFORM 4170-AGE-EFFECTIVE
               PERFORM 4180-EDIT-REJECT
               PERFORM 4190-SELECT-RATE
               PERFORM 4200-SPLIT-RESERVE
               PERFORM 4210-FLAG-AMOUNT-FIN
               PERFORM 4220-SPLIT-PAYMENT
               PERFORM 4230-CHECK-LAST-ACT
               PERFORM 4240-LIMIT-AMOUNT-FIN
               PERFORM 4250-COUNT-SEQ
               PERFORM 4260-LIMIT-ITEM
               PERFORM 4270-ACCRUE-AMOUNT-FIN
               PERFORM 4280-LIMIT-REBATE
               PERFORM 4290-CHECK-NEXT-DUE
               PERFORM 4300-SELECT-MARGIN
               PERFORM 4310-ASSESS-EXEMPT
               PERFORM 4320-FLAG-ITEM
               PERFORM 4330-SPLIT-REBATE
               PERFORM 4340-FLAG-PAYMENT
               PERFORM 4350-COUNT-SEQ
               PERFORM 4360-SELECT-RATE
               PERFORM 4370-SET-CHANNEL
               PERFORM 4380-LIMIT-GAP
               PERFORM 4390-CHECK-NOTICE
               PERFORM 4400-AGE-EXPIRY
               PERFORM 4410-SPLIT-RESERVE
               PERFORM 4420-CHECK-EXPIRY
               PERFORM 4430-SPLIT-AMOUNT-FIN
               PERFORM 4440-ACCRUE-AMOUNT-FIN
               PERFORM 4450-PRORATE-GAP
               PERFORM 4460-ACCRUE-AMOUNT-FIN
               PERFORM 4470-FLAG-RESERVE
               PERFORM 4480-ASSESS-REVIEW
               PERFORM 4490-FLAG-PAYMENT
               PERFORM 4500-EDIT-REF-NO
               PERFORM 4510-ACCRUE-RESERVE
               PERFORM 4520-ACCRUE-RATE
               PERFORM 4530-SET-TYPE
               PERFORM 4540-ACCRUE-MARGIN
               PERFORM 4550-ACCRUE-AMOUNT-FIN
               PERFORM 4560-EDIT-CUST-ID
               PERFORM 4570-SPLIT-CHARGEBK
               PERFORM 4580-ACCUM-CYCLE
               PERFORM 4590-AGE-EXPIRY
               PERFORM 4600-SPLIT-PAYMENT
               PERFORM 4610-PRORATE-REBATE
               PERFORM 7020-CALL-AUDTE
               REWRITE ADL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ADLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ADL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E132 NAME REQUIRED
      * E666 CHARGEBK AMT NEGATIVE
      * E075 CHARGEBK AMT OVER 10000
      * E410 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-NAME = SPACES
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN ADLM-CHARGEBK-AMT < 0
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN ADLM-CHARGEBK-AMT > 10000.00
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN ADLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD < 100.00
               MOVE 100.00 TO ADLM-AMOUNT-FIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-YTD > 50000.00
               MOVE 50000.00 TO ADLM-AMOUNT-FIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E844 DESC REQUIRED
      * E778 AMOUNT FIN AMT NEGATIVE
      * E334 AMOUNT FIN AMT OVER 50000
      * E575 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-DESC = SPACES
                   MOVE 'E844' TO WS-REJECT-CODE
               WHEN ADLM-AMOUNT-FIN-AMT < 0
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN ADLM-AMOUNT-FIN-AMT > 50000.00
                   MOVE 'E334' TO WS-REJECT-CODE
               WHEN ADLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E575' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP BAND TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ADLT-RESERVE-AMT NOT < WS-GAP-BAND-LOW(WS-TX)
                   MOVE WS-GAP-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-PAYMENT-YTD NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0 AND ADLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE REBATE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3080-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLM-PAYMENT-AMT ROUNDED = ADLM-REBATE-AMT *
                       ADLM-MARGIN / 200
               WHEN ADLM-CYCLE-CD = 'A'
                   MOVE 12.00 TO ADLM-PAYMENT-AMT
               WHEN ADLM-CYCLE-CD = 'H'
                   MOVE 10.00 TO ADLM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE ADLM-PAYMENT-AMT ROUNDED = ADLM-REBATE-AMT *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-PAYMENT-AMT > 0 AND ADLM-PAYMENT-AMT < 10.00
               MOVE 10.00 TO ADLM-PAYMENT-AMT
           END-IF
           IF ADLM-PAYMENT-AMT > 250.00
               MOVE 250.00 TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3100-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ADLM-AMOUNT-FIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLM-AMOUNT-FIN-AMT ROUNDED =
                       ADLT-AMOUNT-FIN-AMT * ADLM-RATE / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 10.00 TO ADLM-AMOUNT-FIN-AMT
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 15.00 TO ADLM-AMOUNT-FIN-AMT
               WHEN OTHER
                   COMPUTE ADLM-AMOUNT-FIN-AMT ROUNDED =
                       ADLT-AMOUNT-FIN-AMT * ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-AMOUNT-FIN-AMT > 0 AND ADLM-AMOUNT-FIN-AMT < 2.00
               MOVE 2.00 TO ADLM-AMOUNT-FIN-AMT
           END-IF
           IF ADLM-AMOUNT-FIN-AMT > 25.00
               MOVE 25.00 TO ADLM-AMOUNT-FIN-AMT
           END-IF
           ADD ADLM-AMOUNT-FIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-STATUS-CD
                   ADD ADLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT FIN YTD.
      *----------------------------------------------------------------*
       3120-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED =
                   ADLM-RESERVE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-AMOUNT-FIN-YTD
           END-IF
           ADD ADLM-AMOUNT-FIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESERVE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3130-FLAG-RESERVE.
           IF ADLM-RESERVE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-RESERVE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-REBATE-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-YTD - (WS-WORK-AMT * 4)
           COMPUTE ADLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3170-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ADLM-CHARGEBK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   COMPUTE ADLM-CHARGEBK-YTD ROUNDED = ADLM-PAYMENT-AMT
                       * ADLM-RATE / 200
               WHEN ADLM-DEALER-CD = 'DRU'
                   MOVE 15.00 TO ADLM-CHARGEBK-YTD
               WHEN ADLM-DEALER-CD = 'FNF'
                   MOVE 12.00 TO ADLM-CHARGEBK-YTD
               WHEN OTHER
                   COMPUTE ADLM-CHARGEBK-YTD ROUNDED = ADLM-PAYMENT-AMT
                       * ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-CHARGEBK-YTD > 0 AND ADLM-CHARGEBK-YTD < 2.00
               MOVE 2.00 TO ADLM-CHARGEBK-YTD
           END-IF
           IF ADLM-CHARGEBK-YTD > 150.00
               MOVE 150.00 TO ADLM-CHARGEBK-YTD
           END-IF
           ADD ADLM-CHARGEBK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REBATE YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-REBATE.
           IF ADLM-REBATE-YTD < 0.00
               MOVE 0.00 TO ADLM-REBATE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-REBATE-YTD > 5000.00
               MOVE 5000.00 TO ADLM-REBATE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD ADLM-CHARGEBK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-CHARGEBK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3210-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-GAP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REBATE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-REBATE.
           IF ADLT-REBATE-AMT < 10.00
               MOVE 10.00 TO ADLT-REBATE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLT-REBATE-AMT > 10000.00
               MOVE 10000.00 TO ADLT-REBATE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GAP AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3230-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TYPE-CD
                   ADD ADLM-GAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-GAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3240-FLAG-PAYMENT.
           IF ADLM-PAYMENT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLT-REBATE-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> J WHEN NEXT REVIEW DATE IS PAST
      * J -> T WHEN NEXT REVIEW DATE IS PAST
      * T -> W WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-CYCLE.
           EVALUATE ADLM-CYCLE-CD
               WHEN 'A'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'J' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'T' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REBATE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLT-AMOUNT-FIN-AMT ROUNDED =
                   ADLT-REBATE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLT-AMOUNT-FIN-AMT
           END-IF
           ADD ADLT-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESERVE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3280-FLAG-RESERVE.
           IF ADLM-RESERVE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-RESERVE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN EFFECTIVE DATE IS PAST
      * H -> S WHEN EFFECTIVE DATE IS PAST
      * S -> V WHEN EFFECTIVE DATE IS PAST
      * V -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-REBATE.
           IF ADLT-REBATE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-REBATE-AMT * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESERVE AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-RESERVE-AMT ROUNDED =
                   ADLM-CHARGEBK-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-RESERVE-AMT
           END-IF
           ADD ADLM-RESERVE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-PAYMENT.
           IF ADLM-PAYMENT-AMT < 10.00
               MOVE 10.00 TO ADLM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO ADLM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HMW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RCB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E989 REJECT CD REQUIRED
      * E767 AMOUNT FIN YTD NEGATIVE
      * E610 AMOUNT FIN YTD OVER 5000
      * E562 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-REJECT-CD = SPACES
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN ADLM-AMOUNT-FIN-YTD < 0
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN ADLM-AMOUNT-FIN-YTD > 5000.00
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN ADLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-REBATE.
           IF ADLM-REBATE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-REBATE-YTD * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0 AND ADLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLT-PAYMENT-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLT-REBATE-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GAP AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLM-GAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-GAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE YTD BY DEALER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-DEALER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-DEALER-CD
                   ADD ADLM-RESERVE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-DEALER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESERVE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLM-RESERVE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-RESERVE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ADLM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-STATUS-CD
                   ADD ADLM-CHARGEBK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-CHARGEBK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-BAND-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-GAP-BAND-LOW(WS-TX)
                   MOVE WS-GAP-BAND-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KPP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MVN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UVY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZWZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GAP YTD.
      *----------------------------------------------------------------*
       3490-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-GAP-YTD ROUNDED =
                   ADLM-CHARGEBK-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-GAP-YTD
           END-IF
           ADD ADLM-GAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE GAP BAND TIER FOR THE GAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3500-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ADLM-GAP-YTD NOT < WS-GAP-BAND-LOW(WS-TX)
                   MOVE WS-GAP-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3520-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TIER-CD
                   ADD ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-YTD - (WS-WORK-AMT * 2)
           COMPUTE ADLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-YTD - (WS-WORK-AMT * 3)
           COMPUTE ADLM-RESERVE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE FEE ON THE AMOUNT FIN YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3550-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLT-RESERVE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ADLT-RESERVE-AMT ROUNDED =
                       ADLM-AMOUNT-FIN-YTD * ADLM-RATE / 200
               WHEN ADLM-CYCLE-CD = 'A'
                   MOVE 7.00 TO ADLT-RESERVE-AMT
               WHEN ADLM-CYCLE-CD = 'H'
                   MOVE 12.00 TO ADLT-RESERVE-AMT
               WHEN OTHER
                   COMPUTE ADLT-RESERVE-AMT ROUNDED =
                       ADLM-AMOUNT-FIN-YTD * ADLM-RATE / 100
           END-EVALUATE
           IF ADLT-RESERVE-AMT > 0 AND ADLT-RESERVE-AMT < 5.00
               MOVE 5.00 TO ADLT-RESERVE-AMT
           END-IF
           IF ADLT-RESERVE-AMT > 150.00
               MOVE 150.00 TO ADLT-RESERVE-AMT
           END-IF
           ADD ADLT-RESERVE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-PAYMENT.
           IF ADLT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-PAYMENT-AMT * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLT-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GAP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = ADLM-GAP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-GAP-YTD - (WS-WORK-AMT * 3)
           COMPUTE ADLT-RESERVE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-PAYMENT.
           IF ADLM-PAYMENT-AMT < 0.00
               MOVE 0.00 TO ADLM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO ADLM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-REBATE-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E987 MEMO REQUIRED
      * E966 REBATE AMT NEGATIVE
      * E248 REBATE AMT OVER 250000
      * E500 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-MEMO = SPACES
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN ADLM-REBATE-AMT < 0
                   MOVE 'E966' TO WS-REJECT-CODE
               WHEN ADLM-REBATE-AMT > 250000.00
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN ADLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE FEE ON THE RESERVE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3620-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLT-REBATE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-STOP-FLAG = 'Y'
                   COMPUTE ADLT-REBATE-AMT ROUNDED = ADLM-RESERVE-AMT *
                       ADLM-RATE / 200
               WHEN ADLM-REASON-CD = 'FZV'
                   MOVE 15.00 TO ADLT-REBATE-AMT
               WHEN ADLM-REASON-CD = 'KPP'
                   MOVE 20.00 TO ADLT-REBATE-AMT
               WHEN OTHER
                   COMPUTE ADLT-REBATE-AMT ROUNDED = ADLM-RESERVE-AMT *
                       ADLM-RATE / 100
           END-EVALUATE
           IF ADLT-REBATE-AMT > 0 AND ADLT-REBATE-AMT < 1.00
               MOVE 1.00 TO ADLT-REBATE-AMT
           END-IF
           IF ADLT-REBATE-AMT > 35.00
               MOVE 35.00 TO ADLT-REBATE-AMT
           END-IF
           ADD ADLT-REBATE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ADLM-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ADLM-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGEBK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-CHARGEBK.
           IF ADLM-CHARGEBK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-CHARGEBK-AMT * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E823 DESC REQUIRED
      * E168 WORK AMT NEGATIVE
      * E201 WORK AMT OVER 5000
      * E234 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-DESC = SPACES
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN ADLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE FEE ON THE PAYMENT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3680-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ADLM-RESERVE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE ADLM-RESERVE-AMT ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-RATE / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 10.00 TO ADLM-RESERVE-AMT
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 15.00 TO ADLM-RESERVE-AMT
               WHEN OTHER
                   COMPUTE ADLM-RESERVE-AMT ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-RESERVE-AMT > 0 AND ADLM-RESERVE-AMT < 2.00
               MOVE 2.00 TO ADLM-RESERVE-AMT
           END-IF
           IF ADLM-RESERVE-AMT > 50.00
               MOVE 50.00 TO ADLM-RESERVE-AMT
           END-IF
           ADD ADLM-RESERVE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RESERVE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3690-FLAG-RESERVE.
           IF ADLM-RESERVE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-RESERVE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGEBK YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = ADLM-CHARGEBK-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-CHARGEBK-YTD - (WS-WORK-AMT * 3)
           COMPUTE ADLT-RESERVE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN YTD FEE ON THE RESERVE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3720-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLM-AMOUNT-FIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED =
                       ADLT-RESERVE-AMT * ADLM-FEE-PCT / 200
               WHEN ADLT-TRAN-CD = 'BK'
                   MOVE 3.00 TO ADLM-AMOUNT-FIN-YTD
               WHEN ADLT-TRAN-CD = 'PY'
                   MOVE 7.00 TO ADLM-AMOUNT-FIN-YTD
               WHEN OTHER
                   COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED =
                       ADLT-RESERVE-AMT * ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLM-AMOUNT-FIN-YTD > 0 AND ADLM-AMOUNT-FIN-YTD < 1.00
               MOVE 1.00 TO ADLM-AMOUNT-FIN-YTD
           END-IF
           IF ADLM-AMOUNT-FIN-YTD > 100.00
               MOVE 100.00 TO ADLM-AMOUNT-FIN-YTD
           END-IF
           ADD ADLM-AMOUNT-FIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-PAYMENT.
           IF ADLM-PAYMENT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3740-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-YTD ROUNDED =
                   ADLM-RESERVE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-YTD
           END-IF
           ADD ADLM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TIER-CD
                   ADD ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ADLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3770-LIMIT-CHARGEBK.
           IF ADLM-CHARGEBK-AMT < 25.00
               MOVE 25.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-CHARGEBK-AMT > 500.00
               MOVE 500.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-TRAN-CD
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK YTD BY DEALER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-DEALER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-DEALER-CD
                   ADD ADLM-CHARGEBK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-DEALER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-CHARGEBK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 5
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REBATE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3810-LIMIT-REBATE.
           IF ADLM-REBATE-AMT < 10.00
               MOVE 10.00 TO ADLM-REBATE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-REBATE-AMT > 500.00
               MOVE 500.00 TO ADLM-REBATE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0 AND ADLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0 AND ADLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLT-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-RESERVE.
           IF ADLM-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-AMT * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO ADLM-RESERVE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-STATUS-CD
                   ADD ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3880-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ADLM-RESERVE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   COMPUTE ADLM-RESERVE-YTD ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-RATE / 200
               WHEN ADLM-DEALER-CD = 'DRU'
                   MOVE 3.00 TO ADLM-RESERVE-YTD
               WHEN ADLM-DEALER-CD = 'FNF'
                   MOVE 3.00 TO ADLM-RESERVE-YTD
               WHEN OTHER
                   COMPUTE ADLM-RESERVE-YTD ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-RESERVE-YTD > 0 AND ADLM-RESERVE-YTD < 10.00
               MOVE 10.00 TO ADLM-RESERVE-YTD
           END-IF
           IF ADLM-RESERVE-YTD > 250.00
               MOVE 250.00 TO ADLM-RESERVE-YTD
           END-IF
           ADD ADLM-RESERVE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE ADLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN EFF DATE IS PAST
      * H -> S WHEN EFF DATE IS PAST
      * S -> V WHEN EFF DATE IS PAST
      * V -> W WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RESERVE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3920-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ADLM-RESERVE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ADLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3960-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN LAST MAINT DATE IS PAST
      * JF -> KD WHEN LAST MAINT DATE IS PAST
      * KD -> UG WHEN LAST MAINT DATE IS PAST
      * UG -> WY WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JF' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KD' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WY' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE YTD FEE ON THE CHARGEBK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3990-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ADLM-RESERVE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE ADLM-RESERVE-YTD ROUNDED = ADLM-CHARGEBK-AMT
                       * ADLM-RATE / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 12.00 TO ADLM-RESERVE-YTD
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 12.00 TO ADLM-RESERVE-YTD
               WHEN OTHER
                   COMPUTE ADLM-RESERVE-YTD ROUNDED = ADLM-CHARGEBK-AMT
                       * ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-RESERVE-YTD > 0 AND ADLM-RESERVE-YTD < 5.00
               MOVE 5.00 TO ADLM-RESERVE-YTD
           END-IF
           IF ADLM-RESERVE-YTD > 150.00
               MOVE 150.00 TO ADLM-RESERVE-YTD
           END-IF
           ADD ADLM-RESERVE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT REBATE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-YTD - (WS-WORK-AMT * 12)
           COMPUTE ADLT-RESERVE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4020-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESERVE YTD.
      *----------------------------------------------------------------*
       4030-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-RESERVE-YTD ROUNDED =
                   ADLM-CHARGEBK-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-RESERVE-YTD
           END-IF
           ADD ADLM-RESERVE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-REBATE.
           IF ADLM-REBATE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-REBATE-YTD * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO ADLM-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E062 DESC REQUIRED
      * E870 PAYMENT AMT NEGATIVE
      * E447 PAYMENT AMT OVER 99999
      * E631 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-DESC = SPACES
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN ADLT-PAYMENT-AMT < 0
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN ADLT-PAYMENT-AMT > 99999.00
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN ADLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HMW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RCB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK FEE ON THE GAP YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4080-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLM-CHARGEBK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE ADLM-CHARGEBK-AMT ROUNDED = ADLM-GAP-YTD *
                       ADLM-MARGIN / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 12.00 TO ADLM-CHARGEBK-AMT
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 3.00 TO ADLM-CHARGEBK-AMT
               WHEN OTHER
                   COMPUTE ADLM-CHARGEBK-AMT ROUNDED = ADLM-GAP-YTD *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-CHARGEBK-AMT > 0 AND ADLM-CHARGEBK-AMT < 10.00
               MOVE 10.00 TO ADLM-CHARGEBK-AMT
           END-IF
           IF ADLM-CHARGEBK-AMT > 25.00
               MOVE 25.00 TO ADLM-CHARGEBK-AMT
           END-IF
           ADD ADLM-CHARGEBK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BK -> PY WHEN LAST PAY DATE IS PAST
      * PY -> CB WHEN LAST PAY DATE IS PAST
      * CB -> RB WHEN LAST PAY DATE IS PAST
      * RB -> GP WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4090-SET-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CB'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E931 MEMO REQUIRED
      * E998 PAYMENT YTD NEGATIVE
      * E137 PAYMENT YTD OVER 10000
      * E562 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-MEMO = SPACES
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-YTD < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-YTD > 10000.00
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN ADLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REBATE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4110-FLAG-REBATE.
           IF ADLT-REBATE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLT-REBATE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E633 CONTACT REQUIRED
      * E615 PAYMENT AMT NEGATIVE
      * E526 PAYMENT AMT OVER 250000
      * E731 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-CONTACT = SPACES
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN ADLT-PAYMENT-AMT < 0
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN ADLT-PAYMENT-AMT > 250000.00
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN ADLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E731' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-PAYMENT.
           IF ADLM-PAYMENT-CNT < 9999999
               ADD 1 TO ADLM-PAYMENT-CNT
           END-IF
           IF ADLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLT-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN EFFECTIVE DATE IS PAST
      * KPP -> MVN WHEN EFFECTIVE DATE IS PAST
      * MVN -> UVY WHEN EFFECTIVE DATE IS PAST
      * UVY -> ZWZ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E308 REJECT CD REQUIRED
      * E439 CHARGEBK YTD NEGATIVE
      * E505 CHARGEBK YTD OVER 99999
      * E997 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-REJECT-CD = SPACES
                   MOVE 'E308' TO WS-REJECT-CODE
               WHEN ADLM-CHARGEBK-YTD < 0
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN ADLM-CHARGEBK-YTD > 99999.00
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN ADLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE AMOUNT FIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-AMOUNT-FIN-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESERVE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLM-RESERVE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-RESERVE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ADLM-CHARGEBK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4210-FLAG-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-AMOUNT-FIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE ADLT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4240-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT < 100.00
               MOVE 100.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-AMT > 500.00
               MOVE 500.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4250-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 6
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4260-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT FIN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLM-REBATE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REBATE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-REBATE.
           IF ADLM-REBATE-AMT < 25.00
               MOVE 25.00 TO ADLM-REBATE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-REBATE-AMT > 1000.00
               MOVE 1000.00 TO ADLM-REBATE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-RESERVE-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GAP YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4310-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ADLM-GAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE ADLM-GAP-YTD ROUNDED = WS-WORK-AMT *
                       ADLM-RATE / 200
               WHEN ADLT-TRAN-CD = 'BK'
                   MOVE 7.00 TO ADLM-GAP-YTD
               WHEN ADLT-TRAN-CD = 'PY'
                   MOVE 10.00 TO ADLM-GAP-YTD
               WHEN OTHER
                   COMPUTE ADLM-GAP-YTD ROUNDED = WS-WORK-AMT *
                       ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-GAP-YTD > 0 AND ADLM-GAP-YTD < 2.00
               MOVE 2.00 TO ADLM-GAP-YTD
           END-IF
           IF ADLM-GAP-YTD > 100.00
               MOVE 100.00 TO ADLM-GAP-YTD
           END-IF
           ADD ADLM-GAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4320-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4330-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLT-REBATE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ADLT-REBATE-AMT - (WS-WORK-AMT * 4)
           COMPUTE ADLM-REBATE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4340-FLAG-PAYMENT.
           IF ADLM-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 10
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP BAND TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4360-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ADLM-RESERVE-AMT NOT < WS-GAP-BAND-LOW(WS-TX)
                   MOVE WS-GAP-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4370-SET-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GAP AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4380-LIMIT-GAP.
           IF ADLM-GAP-AMT < 10.00
               MOVE 10.00 TO ADLM-GAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-GAP-AMT > 500.00
               MOVE 500.00 TO ADLM-GAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4400-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ADLT-RESERVE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4420-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = ADLM-AMOUNT-FIN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-AMOUNT-FIN-AMT - (WS-WORK-AMT *
               3)
           COMPUTE ADLM-GAP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT FIN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GAP AMT.
      *----------------------------------------------------------------*
       4450-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-GAP-AMT ROUNDED =
                   ADLM-GAP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-GAP-AMT
           END-IF
           ADD ADLM-GAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT FIN AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4460-ACCRUE-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4470-FLAG-RESERVE.
           IF ADLM-RESERVE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-RESERVE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REBATE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4480-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ADLM-REBATE-AMT *
                       ADLM-FEE-PCT / 200
               WHEN ADLM-STATUS-CD = 'EUA'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN ADLM-STATUS-CD = 'HMW'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ADLM-REBATE-AMT *
                       ADLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4490-FLAG-PAYMENT.
           IF ADLT-PAYMENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E074 REF NO REQUIRED
      * E760 PAYMENT AMT NEGATIVE
      * E031 PAYMENT AMT OVER 1000000
      * E149 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4500-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-REF-NO = SPACES
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-AMT < 0
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-AMT > 1000000.00
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN ADLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-RESERVE.
           IF ADLT-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-RESERVE-AMT * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO ADLT-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4520-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-GAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN NOTICE DATE IS PAST
      * H -> S WHEN NOTICE DATE IS PAST
      * S -> V WHEN NOTICE DATE IS PAST
      * V -> W WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4530-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4540-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT FIN YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4550-ACCRUE-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-AMOUNT-FIN-YTD * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E241 CUST ID REQUIRED
      * E984 AMOUNT FIN AMT NEGATIVE
      * E986 AMOUNT FIN AMT OVER 25000
      * E102 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4560-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-CUST-ID = SPACES
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN ADLT-AMOUNT-FIN-AMT < 0
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN ADLT-AMOUNT-FIN-AMT > 25000.00
                   MOVE 'E986' TO WS-REJECT-CODE
               WHEN ADLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGEBK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4570-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = ADLM-CHARGEBK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ADLM-CHARGEBK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ADLM-REBATE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4580-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4590-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4600-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ADLM-PAYMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ADLM-PAYMENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE ADLT-REBATE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       4610-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-REBATE-AMT ROUNDED =
                   ADLM-AMOUNT-FIN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE ADL FIELDS TO AUDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-AUDTE.
           INITIALIZE AUDTE-PARMS
           MOVE ADLM-PAYMENT-YTD
             TO AUDTE-PAYMENT-YTD
           MOVE ADLM-REBATE-YTD
             TO AUDTE-REBATE-YTD
           MOVE ADLM-REBATE-AMT
             TO AUDTE-REBATE-AMT
           MOVE ADLM-AMOUNT-FIN-AMT
             TO AUDTE-AMOUNT-FIN-AMT
           MOVE ADLM-PAYMENT-AMT
             TO AUDTE-PAYMENT-AMT
           MOVE ADLM-GAP-AMT
             TO AUDTE-GAP-AMT
           MOVE ADLM-NOTICE-DATE
             TO AUDTE-NOTICE-DATE
           MOVE ADLM-LAST-MAINT-DATE
             TO AUDTE-LAST-MAINT-DATE
           MOVE ADLM-EFFECTIVE-DATE
             TO AUDTE-EFFECTIVE-DATE
           MOVE ADLM-EXPIRY-DATE
             TO AUDTE-EXPIRY-DATE
           MOVE ADLM-TYPE-CD
             TO AUDTE-TYPE-CD
           MOVE ADLM-TIER-CD
             TO AUDTE-TIER-CD
           MOVE ADLM-DEALER-CD
             TO AUDTE-DEALER-CD
           MOVE ADLM-CYCLE-CD
             TO AUDTE-CYCLE-CD
           MOVE ADLM-OVERRIDE-FLAG
             TO AUDTE-OVERRIDE-FLAG
           MOVE ADLM-NOTICE-SENT-FLAG
             TO AUDTE-NOTICE-SENT-FLAG
           MOVE ADLM-REVIEW-FLAG
             TO AUDTE-REVIEW-FLAG
           MOVE ADLM-RETRY-CNT
             TO AUDTE-RETRY-CNT
           MOVE ADLM-EXCEPT-CNT
             TO AUDTE-EXCEPT-CNT
           MOVE ADLM-FEE-PCT
             TO AUDTE-FEE-PCT
           MOVE ADLM-MARGIN
             TO AUDTE-MARGIN
           MOVE ADLM-DESC
             TO AUDTE-DESC
           MOVE ADLM-NAME
             TO AUDTE-NAME
           CALL 'AUDTE' USING AUDTE-PARMS
           EVALUATE AUDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE AUDTE-PAYMENT-YTD
                     TO ADLM-PAYMENT-YTD
                   MOVE AUDTE-REBATE-YTD
                     TO ADLM-REBATE-YTD
                   MOVE AUDTE-AMOUNT-FIN-AMT
                     TO ADLM-AMOUNT-FIN-AMT
                   MOVE AUDTE-PAYMENT-AMT
                     TO ADLM-PAYMENT-AMT
                   MOVE AUDTE-GAP-AMT
                     TO ADLM-GAP-AMT
                   MOVE AUDTE-REVIEW-FLAG
                     TO ADLM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-AUDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ADLT-KEY TO RD-KEY
           MOVE ADLT-TRAN-CD TO RD-CODE
           MOVE ADLT-PAYMENT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ADLT-KEY TO RD-KEY
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
           DISPLAY 'AULPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'AULPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
