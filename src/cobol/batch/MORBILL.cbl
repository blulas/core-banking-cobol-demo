      *================================================================*
      * PROGRAM  : MORBILL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MONEY ORDERS - CYCLE BILLING. READS THE MOR         *
      *            TRANSACTIONS, UPDATES THE MOR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. MONEY ORDER AND OFFICIAL CHECK  *
      *            ISSUANCE, PAID ITEMS AND ESCHEAT                    *
      * INPUT    : MORMST (KSDS), MORTRN, BANKCTL                      *
      * OUTPUT   : MOROUT, MORBIRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MODTE, MOFEE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-10-05 PQR  ORIGINAL                                       *
      * 1993-01-19 SKN  PERFORMANCE - OUTSTAND READ AHEAD INC0412      *
      * 1993-11-22 LMB  NEW ESCHEAT REPORT COLUMNS INC1883             *
      * 1996-12-13 CRP  VOIDED CALCULATION CORRECTED PRB1808           *
      * 1997-11-04 SKN  VOIDED EXCEPTIONS TO OPS QUEUE CHG1122         *
      * 1998-05-03 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-02-09 PQR  OUTSTAND CUTOFF MOVED REQ6195                  *
      * 2003-09-06 EWS  PAID ROUNDING - HALF UP REQ0485                *
      * 2006-11-05 LMB  FEE STATUS CODES ADDED PRB1980                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORBILL.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "MORMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MORM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "MORTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "MOROUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "MORBIRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY MORMST.
       FD  TRAN-FILE.
       COPY MORTRN.
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
      * WS-ESCHEAT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ESCHEAT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000348000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000462000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000599000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000030000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000217000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000812000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020300000000084000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025300000000709000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025400000000073000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025450000000679500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035450000000886000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040450000000734500'.
       01  WS-ESCHEAT-SCHED-TBL REDEFINES WS-ESCHEAT-SCHED-VALUES.
           05  WS-ESCHEAT-SCHED-ROW OCCURS 12 TIMES.
               10  WS-ESCHEAT-SCHED-LOW    PIC 9(09)V99.
               10  WS-ESCHEAT-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000254000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000225500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000552500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000086000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000350000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008650000000889500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013650000000154000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016150000000602500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016200000000600500'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 10 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-OUTSTAND-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000695000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000070000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000047000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000628500'.
       01  WS-OUTSTAND-GRID-TBL REDEFINES WS-OUTSTAND-GRID-VALUES.
           05  WS-OUTSTAND-GRID-ROW OCCURS 4 TIMES.
               10  WS-OUTSTAND-GRID-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FACE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000347000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000082000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000309500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000332000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000014000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000339000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026250000000040000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000146000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031350000000288500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031400000000255500'.
       01  WS-FACE-TIER-TBL REDEFINES WS-FACE-TIER-VALUES.
           05  WS-FACE-TIER-ROW OCCURS 10 TIMES.
               10  WS-FACE-TIER-LOW        PIC 9(09)V99.
               10  WS-FACE-TIER-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MORBILL  MONEY ORDERS'.
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
       COPY MODTEP.
       COPY MOFEEP.

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
               MOVE 'MORMST' TO ABND-FILE-NAME
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
           MOVE MORT-KEY TO MORM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-TYPE
               PERFORM 3010-ACCRUE-OUTSTAND
               PERFORM 3020-FLAG-FACE
               PERFORM 3030-ASSESS-REVIEW
               PERFORM 3040-ACCRUE-VOIDED
               PERFORM 3050-EDIT-DESC
               PERFORM 3060-AGE-LAST-MAINT
               PERFORM 3070-FLAG-FACE
               PERFORM 3080-ASSESS-EXEMPT
               PERFORM 3090-SPLIT-ITEM
               PERFORM 3100-ASSESS-PREFERRED
               PERFORM 3110-SELECT-FEE-PCT
               PERFORM 3120-FLAG-ITEM
               PERFORM 3130-SELECT-DISCOUNT
               PERFORM 3140-CHECK-EXPIRY
               PERFORM 3150-EDIT-CONTACT
               PERFORM 3160-ACCUM-CHANNEL
               PERFORM 3170-SELECT-PENALTY-PCT
               PERFORM 3180-SET-REASON
               PERFORM 3190-SPLIT-VOIDED
               PERFORM 3200-SET-CYCLE
               PERFORM 3210-SET-CHANNEL
               PERFORM 3220-SPLIT-VOIDED
               PERFORM 3230-SELECT-DISCOUNT
               PERFORM 3240-DESCRIBE-REASON
               PERFORM 3250-EDIT-REF-NO
               PERFORM 3260-CHECK-LAST-ACT
               PERFORM 3270-PRORATE-FACE
               PERFORM 3280-FLAG-ESCHEAT
               PERFORM 3290-SELECT-DISCOUNT
               PERFORM 3300-PRORATE-OUTSTAND
               PERFORM 3310-SPLIT-FACE
               PERFORM 3320-EDIT-CONTACT
               PERFORM 3330-EDIT-ADDR
               PERFORM 3340-SPLIT-FEE
               PERFORM 3350-CHECK-LAST-ACT
               PERFORM 3360-ACCRUE-OUTSTAND
               PERFORM 3370-SET-TYPE
               PERFORM 3380-ACCRUE-ESCHEAT
               PERFORM 3390-AGE-EXPIRY
               PERFORM 3400-EDIT-DESC
               PERFORM 3410-ACCRUE-FACE
               PERFORM 3420-DESCRIBE-CHANNEL
               PERFORM 3430-ACCRUE-FACE
               PERFORM 3440-SET-PAYEE-TYPE
               PERFORM 3450-ASSESS-OVERRIDE
               PERFORM 3460-AGE-EXPIRY
               PERFORM 3470-DESCRIBE-PAYEE-TYPE
               PERFORM 3480-COUNT-PAYMENT
               PERFORM 3490-ASSESS-REVERSAL
               PERFORM 3500-DESCRIBE-ITEM-TYPE
               PERFORM 3510-FLAG-FACE
               PERFORM 3520-AGE-POST
               PERFORM 3530-AGE-LAST-PAY
               PERFORM 3540-FLAG-OUTSTAND
               PERFORM 3550-ASSESS-PREFERRED
               PERFORM 3560-ACCRUE-ESCHEAT
               PERFORM 3570-PRORATE-OUTSTAND
               PERFORM 3580-AGE-NOTICE
               PERFORM 3590-AGE-POST
               PERFORM 3600-ACCRUE-OUTSTAND
               PERFORM 3610-COUNT-SEQ
               PERFORM 3620-ACCUM-TYPE
               PERFORM 3630-SET-REASON
               PERFORM 3640-LIMIT-FEE
               PERFORM 3650-PRORATE-FEE
               PERFORM 3660-SELECT-PENALTY-PCT
               PERFORM 3670-COUNT-ITEM
               PERFORM 3680-SET-REASON
               PERFORM 3690-CHECK-LAST-PAY
               PERFORM 3700-PRORATE-ESCHEAT
               PERFORM 3710-SPLIT-FEE
               PERFORM 3720-SPLIT-FEE
               PERFORM 3730-ACCRUE-ESCHEAT
               PERFORM 3740-LIMIT-FEE
               PERFORM 3750-AGE-POST
               PERFORM 3760-ACCRUE-OUTSTAND
               PERFORM 3770-DESCRIBE-TYPE
               PERFORM 3780-DESCRIBE-PAYEE-TYPE
               PERFORM 3790-FLAG-OUTSTAND
               PERFORM 3800-FLAG-FACE
               PERFORM 3810-COUNT-NOTICE
               PERFORM 3820-EDIT-ADDR
               PERFORM 3830-ACCUM-TRAN
               PERFORM 3840-DESCRIBE-PAYEE-TYPE
               PERFORM 3850-ACCUM-ITEM-TYPE
               PERFORM 3860-LIMIT-ESCHEAT
               PERFORM 3870-LIMIT-PAID
               PERFORM 3880-SELECT-DISCOUNT
               PERFORM 3890-ACCUM-CYCLE
               PERFORM 3900-EDIT-ADDR
               PERFORM 3910-EDIT-NAME
               PERFORM 3920-FLAG-VOIDED
               PERFORM 3930-LIMIT-ESCHEAT
               PERFORM 3940-ASSESS-STOP
               PERFORM 3950-CHECK-POST
               PERFORM 3960-DESCRIBE-PAYEE-TYPE
               PERFORM 3970-ASSESS-REVERSAL
               PERFORM 3980-CHECK-EXPIRY
               PERFORM 3990-ACCUM-TRAN
               PERFORM 4000-COUNT-RETRY
               PERFORM 4010-ASSESS-EXEMPT
               PERFORM 4020-AGE-POST
               PERFORM 4030-EDIT-CONTACT
               PERFORM 4040-SPLIT-PAID
               PERFORM 4050-EDIT-REF-NO
               PERFORM 4060-ACCRUE-FEE
               PERFORM 4070-ACCUM-TYPE
               PERFORM 4080-ACCRUE-OUTSTAND
               PERFORM 4090-SET-ITEM-TYPE
               PERFORM 4100-FLAG-PAID
               PERFORM 4110-PRORATE-FEE
               PERFORM 4120-SPLIT-VOIDED
               PERFORM 4130-SET-TYPE
               PERFORM 4140-DESCRIBE-CYCLE
               PERFORM 4150-ACCRUE-VOIDED
               PERFORM 4160-ACCUM-REASON
               PERFORM 4170-COUNT-RETRY
               PERFORM 4180-ACCRUE-FEE
               PERFORM 4190-SPLIT-ESCHEAT
               PERFORM 4200-AGE-CLOSE
               PERFORM 4210-CHECK-NOTICE
               PERFORM 4220-AGE-LAST-PAY
               PERFORM 4230-SELECT-PENALTY-PCT
               PERFORM 4240-PRORATE-FEE
               PERFORM 4250-PRORATE-VOIDED
               PERFORM 7020-CALL-MODTE
               PERFORM 7000-CALL-MOFEE
               REWRITE MOR-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'MORMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM MOR-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3020-FLAG-FACE.
           IF MORM-FACE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3030-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MORM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MORT-ESCHEAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = WS-WORK-AMT-2 *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-PAYEE-TYPE-CD = 'CJ'
                   MOVE 10.00 TO MORT-ESCHEAT-AMT
               WHEN MORM-PAYEE-TYPE-CD = 'FY'
                   MOVE 15.00 TO MORT-ESCHEAT-AMT
               WHEN OTHER
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = WS-WORK-AMT-2 *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORT-ESCHEAT-AMT > 0 AND MORT-ESCHEAT-AMT < 5.00
               MOVE 5.00 TO MORT-ESCHEAT-AMT
           END-IF
           IF MORT-ESCHEAT-AMT > 35.00
               MOVE 35.00 TO MORT-ESCHEAT-AMT
           END-IF
           ADD MORT-ESCHEAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOIDED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-VOIDED.
           IF MORM-VOIDED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-VOIDED-AMT * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORT-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E045 DESC REQUIRED
      * E977 FACE YTD NEGATIVE
      * E630 FACE YTD OVER 25000
      * E450 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN MORM-FACE-YTD < 0
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN MORM-FACE-YTD > 25000.00
                   MOVE 'E630' TO WS-REJECT-CODE
               WHEN MORM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3070-FLAG-FACE.
           IF MORT-FACE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORT-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3080-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-STOP-FLAG = 'Y'
                   COMPUTE MORM-PAID-YTD ROUNDED = WS-WORK-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORM-CYCLE-CD = 'DGU'
                   MOVE 5.00 TO MORM-PAID-YTD
               WHEN MORM-CYCLE-CD = 'GEJ'
                   MOVE 7.00 TO MORM-PAID-YTD
               WHEN OTHER
                   COMPUTE MORM-PAID-YTD ROUNDED = WS-WORK-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-PAID-YTD > 0 AND MORM-PAID-YTD < 5.00
               MOVE 5.00 TO MORM-PAID-YTD
           END-IF
           IF MORM-PAID-YTD > 150.00
               MOVE 150.00 TO MORM-PAID-YTD
           END-IF
           ADD MORM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3100-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORM-PAID-AMT ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 10.00 TO MORM-PAID-AMT
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 15.00 TO MORM-PAID-AMT
               WHEN OTHER
                   COMPUTE MORM-PAID-AMT ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-PAID-AMT > 0 AND MORM-PAID-AMT < 5.00
               MOVE 5.00 TO MORM-PAID-AMT
           END-IF
           IF MORM-PAID-AMT > 100.00
               MOVE 100.00 TO MORM-PAID-AMT
           END-IF
           ADD MORM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT SCHED TIER FOR THE VOIDED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-SCHED-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MORM-VOIDED-YTD NOT < WS-ESCHEAT-SCHED-LOW(WS-TX)
                   MOVE WS-ESCHEAT-SCHED-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3120-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE OUTSTAND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF MORM-OUTSTAND-YTD NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E994 CONTACT REQUIRED
      * E374 WORK AMT NEGATIVE
      * E524 WORK AMT OVER 1000000
      * E810 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3150-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN MORM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN LAST ACT DATE IS PAST
      * L -> P WHEN LAST ACT DATE IS PAST
      * P -> S WHEN LAST ACT DATE IS PAST
      * S -> T WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOIDED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN LAST STMT DATE IS PAST
      * GEJ -> JBE WHEN LAST STMT DATE IS PAST
      * JBE -> DGU WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN LAST PAY DATE IS PAST
      * DX -> PT WHEN LAST PAY DATE IS PAST
      * PT -> RN WHEN LAST PAY DATE IS PAST
      * RN -> YD WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOIDED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-AMT - (WS-WORK-AMT * 12)
           COMPUTE MORT-ESCHEAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E145 REF NO REQUIRED
      * E637 FACE YTD NEGATIVE
      * E526 FACE YTD OVER 25000
      * E505 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-REF-NO = SPACES
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN MORM-FACE-YTD < 0
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN MORM-FACE-YTD > 25000.00
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3260-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-FACE-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORT-FACE-AMT
           END-IF
           ADD MORT-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ESCHEAT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3280-FLAG-ESCHEAT.
           IF MORM-ESCHEAT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-ESCHEAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF MORT-OUTSTAND-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3300-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-OUTSTAND-YTD ROUNDED =
                   MORT-FACE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-OUTSTAND-YTD
           END-IF
           ADD MORM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = MORM-FACE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORM-FACE-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORT-ESCHEAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E428 CONTACT REQUIRED
      * E596 WORK AMT 2 NEGATIVE
      * E422 WORK AMT 2 OVER 25000
      * E489 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E422' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E142 ADDR REQUIRED
      * E584 FACE AMT NEGATIVE
      * E284 FACE AMT OVER 10000
      * E111 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-ADDR = SPACES
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT < 0
                   MOVE 'E584' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT > 10000.00
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN MORM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MORM-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-AMT * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN NOTICE DATE IS PAST
      * JDX -> LDX WHEN NOTICE DATE IS PAST
      * LDX -> RHT WHEN NOTICE DATE IS PAST
      * RHT -> AGE WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * MORM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MORM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E352 DESC REQUIRED
      * E074 ESCHEAT AMT NEGATIVE
      * E411 ESCHEAT AMT OVER 5000
      * E396 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT < 0
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT > 5000.00
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN MORM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-FACE.
           IF MORM-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-FACE.
           IF MORM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-YTD * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN LAST STMT DATE IS PAST
      * FY -> HX WHEN LAST STMT DATE IS PAST
      * HX -> UV WHEN LAST STMT DATE IS PAST
      * UV -> VV WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3450-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MORM-FEE-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN MORT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MORM-FEE-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO MORM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT YTD FEE ON THE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3490-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MORM-ESCHEAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORM-ESCHEAT-YTD ROUNDED = MORM-FEE-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORM-REASON-CD = 'B'
                   MOVE 5.00 TO MORM-ESCHEAT-YTD
               WHEN MORM-REASON-CD = 'L'
                   MOVE 20.00 TO MORM-ESCHEAT-YTD
               WHEN OTHER
                   COMPUTE MORM-ESCHEAT-YTD ROUNDED = MORM-FEE-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORM-ESCHEAT-YTD > 0 AND MORM-ESCHEAT-YTD < 2.00
               MOVE 2.00 TO MORM-ESCHEAT-YTD
           END-IF
           IF MORM-ESCHEAT-YTD > 35.00
               MOVE 35.00 TO MORM-ESCHEAT-YTD
           END-IF
           ADD MORM-ESCHEAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3510-FLAG-FACE.
           IF MORT-FACE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORT-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0 AND MORT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-OUTSTAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE FACE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3550-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORT-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORT-FACE-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 7.00 TO MORT-OUTSTAND-AMT
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 3.00 TO MORT-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORT-FACE-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORT-OUTSTAND-AMT > 0 AND MORT-OUTSTAND-AMT < 5.00
               MOVE 5.00 TO MORT-OUTSTAND-AMT
           END-IF
           IF MORT-OUTSTAND-AMT > 100.00
               MOVE 100.00 TO MORT-OUTSTAND-AMT
           END-IF
           ADD MORT-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORT-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0 AND MORT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-SEQ.
           IF MORT-SEQ < 9999999
               ADD 1 TO MORT-SEQ
           END-IF
           IF MORT-SEQ NOT < 5
               MOVE 'Y' TO MORM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN NOTICE DATE IS PAST
      * L -> P WHEN NOTICE DATE IS PAST
      * P -> S WHEN NOTICE DATE IS PAST
      * S -> T WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-FEE.
           IF MORM-FEE-AMT < 100.00
               MOVE 100.00 TO MORM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FEE-AMT > 10000.00
               MOVE 10000.00 TO MORM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3650-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MORM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF MORT-FACE-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-ITEM.
           IF MORM-ITEM-CNT < 9999999
               ADD 1 TO MORM-ITEM-CNT
           END-IF
           IF MORM-ITEM-CNT NOT < 10
               MOVE 'Y' TO MORM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN CLOSE DATE IS PAST
      * L -> P WHEN CLOSE DATE IS PAST
      * P -> S WHEN CLOSE DATE IS PAST
      * S -> T WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3690-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ESCHEAT YTD.
      *----------------------------------------------------------------*
       3700-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-ESCHEAT-YTD ROUNDED =
                   MORT-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-ESCHEAT-YTD
           END-IF
           ADD MORM-ESCHEAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MORT-ESCHEAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MORM-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-ESCHEAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-FEE.
           IF MORT-FEE-AMT < 10.00
               MOVE 10.00 TO MORT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FEE-AMT > 50000.00
               MOVE 50000.00 TO MORT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0 AND MORT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-OUTSTAND.
           IF MORT-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-OUTSTAND-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JDX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LDX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RHT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3790-FLAG-OUTSTAND.
           IF MORM-OUTSTAND-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-FACE.
           IF MORM-FACE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E066 ADDR REQUIRED
      * E694 FACE AMT NEGATIVE
      * E688 FACE AMT OVER 10000
      * E017 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-ADDR = SPACES
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT < 0
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT > 10000.00
                   MOVE 'E688' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-TRAN-CD
                   ADD MORT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT YTD BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-ITEM-TYPE-CD
                   ADD MORM-ESCHEAT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-ESCHEAT.
           IF MORT-ESCHEAT-AMT < 10.00
               MOVE 10.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-ESCHEAT-AMT > 10000.00
               MOVE 10000.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3870-LIMIT-PAID.
           IF MORM-PAID-AMT < 0.00
               MOVE 0.00 TO MORM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-PAID-AMT > 500.00
               MOVE 500.00 TO MORM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND GRID TIER FOR THE VOIDED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-GRID-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MORM-VOIDED-YTD NOT < WS-OUTSTAND-GRID-LOW(WS-TX)
                   MOVE WS-OUTSTAND-GRID-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-CYCLE-CD
                   ADD MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E990 ADDR REQUIRED
      * E447 FEE AMT NEGATIVE
      * E797 FEE AMT OVER 5000
      * E794 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-ADDR = SPACES
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN MORM-FEE-AMT < 0
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN MORM-FEE-AMT > 5000.00
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN MORM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E501 NAME REQUIRED
      * E624 ESCHEAT YTD NEGATIVE
      * E537 ESCHEAT YTD OVER 25000
      * E897 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-NAME = SPACES
                   MOVE 'E501' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-YTD < 0
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-YTD > 25000.00
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOIDED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3920-FLAG-VOIDED.
           IF MORM-VOIDED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-VOIDED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-YTD < 50.00
               MOVE 50.00 TO MORM-ESCHEAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-YTD > 50000.00
               MOVE 50000.00 TO MORM-ESCHEAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE PAID YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3940-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORM-FACE-YTD ROUNDED = MORM-PAID-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-TYPE-CD = 'AGE'
                   MOVE 5.00 TO MORM-FACE-YTD
               WHEN MORM-TYPE-CD = 'JDX'
                   MOVE 15.00 TO MORM-FACE-YTD
               WHEN OTHER
                   COMPUTE MORM-FACE-YTD ROUNDED = MORM-PAID-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-FACE-YTD > 0 AND MORM-FACE-YTD < 1.00
               MOVE 1.00 TO MORM-FACE-YTD
           END-IF
           IF MORM-FACE-YTD > 25.00
               MOVE 25.00 TO MORM-FACE-YTD
           END-IF
           ADD MORM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3960-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3970-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MORM-FACE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORM-FACE-AMT ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORM-CYCLE-CD = 'DGU'
                   MOVE 3.00 TO MORM-FACE-AMT
               WHEN MORM-CYCLE-CD = 'GEJ'
                   MOVE 5.00 TO MORM-FACE-AMT
               WHEN OTHER
                   COMPUTE MORM-FACE-AMT ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORM-FACE-AMT > 0 AND MORM-FACE-AMT < 1.00
               MOVE 1.00 TO MORM-FACE-AMT
           END-IF
           IF MORM-FACE-AMT > 50.00
               MOVE 50.00 TO MORM-FACE-AMT
           END-IF
           ADD MORM-FACE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-TRAN-CD
                   ADD MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 6
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE FEE ON THE VOIDED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4010-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORM-FACE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORM-FACE-AMT ROUNDED = MORM-VOIDED-YTD *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 5.00 TO MORM-FACE-AMT
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 10.00 TO MORM-FACE-AMT
               WHEN OTHER
                   COMPUTE MORM-FACE-AMT ROUNDED = MORM-VOIDED-YTD *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-FACE-AMT > 0 AND MORM-FACE-AMT < 5.00
               MOVE 5.00 TO MORM-FACE-AMT
           END-IF
           IF MORM-FACE-AMT > 35.00
               MOVE 35.00 TO MORM-FACE-AMT
           END-IF
           ADD MORM-FACE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0 AND MORT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E966 CONTACT REQUIRED
      * E765 FACE YTD NEGATIVE
      * E978 FACE YTD OVER 5000
      * E598 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E966' TO WS-REJECT-CODE
               WHEN MORM-FACE-YTD < 0
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN MORM-FACE-YTD > 5000.00
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN MORM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = MORM-PAID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORM-PAID-AMT - (WS-WORK-AMT * 2)
           COMPUTE MORM-VOIDED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E377 REF NO REQUIRED
      * E069 WORK AMT NEGATIVE
      * E743 WORK AMT OVER 99999
      * E230 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-REF-NO = SPACES
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E743' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-FEE.
           IF MORT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-FEE-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-OUTSTAND.
           IF MORT-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-OUTSTAND-AMT * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN EFF DATE IS PAST
      * H -> S WHEN EFF DATE IS PAST
      * S -> T WHEN EFF DATE IS PAST
      * T -> B WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4090-SET-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4100-FLAG-PAID.
           IF MORM-PAID-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4110-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-FEE-AMT ROUNDED =
                   MORM-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT VOIDED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-AMT - (WS-WORK-AMT * 2)
           COMPUTE MORM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN EFF DATE IS PAST
      * JDX -> LDX WHEN EFF DATE IS PAST
      * LDX -> RHT WHEN EFF DATE IS PAST
      * RHT -> AGE WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOIDED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-VOIDED.
           IF MORM-VOIDED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-VOIDED-AMT * MORM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MORM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-REASON-CD
                   ADD MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 6
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-FEE.
           IF MORM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FEE-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-VOIDED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MORM-ESCHEAT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORM-ESCHEAT-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORT-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MORM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4240-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-FEE-YTD ROUNDED =
                   MORM-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-FEE-YTD
           END-IF
           ADD MORM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VOIDED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOIDED AMT.
      *----------------------------------------------------------------*
       4250-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-VOIDED-AMT ROUNDED =
                   MORM-VOIDED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE MOR FIELDS TO MODTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-MODTE.
           INITIALIZE MODTE-PARMS
           MOVE MORM-FEE-YTD
             TO MODTE-FEE-YTD
           MOVE MORM-FEE-AMT
             TO MODTE-FEE-AMT
           MOVE MORM-PAID-AMT
             TO MODTE-PAID-AMT
           MOVE MORM-VOIDED-AMT
             TO MODTE-VOIDED-AMT
           MOVE MORM-ESCHEAT-AMT
             TO MODTE-ESCHEAT-AMT
           MOVE MORM-FACE-AMT
             TO MODTE-FACE-AMT
           MOVE MORM-LAST-MAINT-DATE
             TO MODTE-LAST-MAINT-DATE
           MOVE MORM-EXPIRY-DATE
             TO MODTE-EXPIRY-DATE
           MOVE MORM-CLOSE-DATE
             TO MODTE-CLOSE-DATE
           MOVE MORM-OPEN-DATE
             TO MODTE-OPEN-DATE
           MOVE MORM-ITEM-TYPE-CD
             TO MODTE-ITEM-TYPE-CD
           MOVE MORM-REASON-CD
             TO MODTE-REASON-CD
           MOVE MORM-TYPE-CD
             TO MODTE-TYPE-CD
           MOVE MORM-CYCLE-CD
             TO MODTE-CYCLE-CD
           MOVE MORM-STOP-FLAG
             TO MODTE-STOP-FLAG
           MOVE MORM-AUTO-RENEW-FLAG
             TO MODTE-AUTO-RENEW-FLAG
           MOVE MORM-EXEMPT-FLAG
             TO MODTE-EXEMPT-FLAG
           MOVE MORM-PAYMENT-CNT
             TO MODTE-PAYMENT-CNT
           MOVE MORM-ITEM-CNT
             TO MODTE-ITEM-CNT
           MOVE MORM-FEE-PCT
             TO MODTE-FEE-PCT
           MOVE MORM-DISCOUNT
             TO MODTE-DISCOUNT
           MOVE MORM-NAME
             TO MODTE-NAME
           MOVE MORM-REF-NO
             TO MODTE-REF-NO
           CALL 'MODTE' USING MODTE-PARMS
           EVALUATE MODTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MODTE-FEE-AMT
                     TO MORM-FEE-AMT
                   MOVE MODTE-VOIDED-AMT
                     TO MORM-VOIDED-AMT
                   MOVE MODTE-ESCHEAT-AMT
                     TO MORM-ESCHEAT-AMT
                   MOVE MODTE-FACE-AMT
                     TO MORM-FACE-AMT
                   MOVE MODTE-STOP-FLAG
                     TO MORM-STOP-FLAG
                   MOVE MODTE-AUTO-RENEW-FLAG
                     TO MORM-AUTO-RENEW-FLAG
                   MOVE MODTE-EXEMPT-FLAG
                     TO MORM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-MODTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MOR FIELDS TO MOFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-MOFEE.
           INITIALIZE MOFEE-PARMS
           MOVE MORM-OUTSTAND-AMT
             TO MOFEE-OUTSTAND-AMT
           MOVE MORM-ESCHEAT-AMT
             TO MOFEE-ESCHEAT-AMT
           MOVE MORM-PAID-YTD
             TO MOFEE-PAID-YTD
           MOVE MORM-PAID-AMT
             TO MOFEE-PAID-AMT
           MOVE MORM-FACE-AMT
             TO MOFEE-FACE-AMT
           MOVE MORM-FEE-AMT
             TO MOFEE-FEE-AMT
           MOVE MORM-LAST-STMT-DATE
             TO MOFEE-LAST-STMT-DATE
           MOVE MORM-LAST-PAY-DATE
             TO MOFEE-LAST-PAY-DATE
           MOVE MORM-LAST-ACT-DATE
             TO MOFEE-LAST-ACT-DATE
           MOVE MORM-EFFECTIVE-DATE
             TO MOFEE-EFFECTIVE-DATE
           MOVE MORM-PAYEE-TYPE-CD
             TO MOFEE-PAYEE-TYPE-CD
           MOVE MORM-CHANNEL-CD
             TO MOFEE-CHANNEL-CD
           MOVE MORM-REASON-CD
             TO MOFEE-REASON-CD
           MOVE MORM-ITEM-TYPE-CD
             TO MOFEE-ITEM-TYPE-CD
           MOVE MORM-OVERRIDE-FLAG
             TO MOFEE-OVERRIDE-FLAG
           MOVE MORM-STOP-FLAG
             TO MOFEE-STOP-FLAG
           MOVE MORM-PREFERRED-FLAG
             TO MOFEE-PREFERRED-FLAG
           MOVE MORM-ITEM-CNT
             TO MOFEE-ITEM-CNT
           MOVE MORM-RETRY-CNT
             TO MOFEE-RETRY-CNT
           MOVE MORM-DISCOUNT
             TO MOFEE-DISCOUNT
           MOVE MORM-FEE-PCT
             TO MOFEE-FEE-PCT
           MOVE MORM-REF-NO
             TO MOFEE-REF-NO
           MOVE MORM-NAME
             TO MOFEE-NAME
           CALL 'MOFEE' USING MOFEE-PARMS
           EVALUATE MOFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MOFEE-OUTSTAND-AMT
                     TO MORM-OUTSTAND-AMT
                   MOVE MOFEE-PAID-YTD
                     TO MORM-PAID-YTD
                   MOVE MOFEE-FACE-AMT
                     TO MORM-FACE-AMT
                   MOVE MOFEE-STOP-FLAG
                     TO MORM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-MOFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE MORT-KEY TO RD-KEY
           MOVE MORT-TRAN-CD TO RD-CODE
           MOVE MORT-FACE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE MORT-KEY TO RD-KEY
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
           DISPLAY 'MORBILL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MORBILL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
