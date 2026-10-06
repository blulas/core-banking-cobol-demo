      *================================================================*
      * PROGRAM  : HELNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : HOME EQUITY LINES - CUSTOMER NOTICES. READS THE HEL *
      *            TRANSACTIONS, UPDATES THE HEL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOME EQUITY LINE DRAWS, BILLING,*
      *            RATE CHANGE AND FREEZE                              *
      * INPUT    : HELMST (KSDS), HELTRN, BANKCTL                      *
      * OUTPUT   : HELOUT, HELNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, HESTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-11 GAF  ORIGINAL                                       *
      * 1995-12-08 EWS  NEW INTEREST REPORT COLUMNS INC0850            *
      * 1999-12-27 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-26 CRP  PERFORMANCE - BALANCE READ AHEAD REQ2316       *
      * 2004-03-10 LMB  PERFORMANCE - DRAW READ AHEAD PRB1863          *
      * 2004-08-21 GAF  BALANCE ROUNDING - HALF UP CHG2486             *
      * 2005-07-19 CRP  MIN-PAY FEE SCHEDULE UPDATE CHG9905            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELNOTC.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "HELMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS HELM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "HELTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "HELOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "HELNORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY HELMST.
       FD  TRAN-FILE.
       COPY HELTRN.
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
                                           VALUE 9.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000232500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000207000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000750000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000840000'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 4 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MIN-PAY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000373000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000602500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000407000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000315000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000093500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022600000000035500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000093500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023650000000264500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024650000000794000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024700000000210000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024950000000067000'.
       01  WS-MIN-PAY-GRID-TBL REDEFINES WS-MIN-PAY-GRID-VALUES.
           05  WS-MIN-PAY-GRID-ROW OCCURS 11 TIMES.
               10  WS-MIN-PAY-GRID-LOW     PIC 9(09)V99.
               10  WS-MIN-PAY-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MIN-PAY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000132000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000813500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000311000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000725500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000031000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000550000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020600000000534000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020850000000662500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030850000000581000'.
       01  WS-MIN-PAY-TIER-TBL REDEFINES WS-MIN-PAY-TIER-VALUES.
           05  WS-MIN-PAY-TIER-ROW OCCURS 9 TIMES.
               10  WS-MIN-PAY-TIER-LOW     PIC 9(09)V99.
               10  WS-MIN-PAY-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MIN-PAY-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000761500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000854000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000828500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000177500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000374000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000109000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000321500'.
       01  WS-MIN-PAY-TIERX-TBL REDEFINES WS-MIN-PAY-TIERX-VALUES.
           05  WS-MIN-PAY-TIERX-ROW OCCURS 7 TIMES.
               10  WS-MIN-PAY-TIERX-LOW    PIC 9(09)V99.
               10  WS-MIN-PAY-TIERX-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  HELNOTC  HOME EQUITY LINES'.
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
       COPY HESTATP.

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
               MOVE 'HELMST' TO ABND-FILE-NAME
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
           MOVE HELT-KEY TO HELM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-ITEM
               PERFORM 3010-AGE-POST
               PERFORM 3020-SPLIT-LIMIT
               PERFORM 3030-ASSESS-AUTO-RENEW
               PERFORM 3040-EDIT-DESC
               PERFORM 3050-PRORATE-INTEREST
               PERFORM 3060-FLAG-BALANCE
               PERFORM 3070-SELECT-PENALTY-PCT
               PERFORM 3080-LIMIT-ANNUAL-FEE
               PERFORM 3090-EDIT-REF-NO
               PERFORM 3100-SPLIT-APPRAISE
               PERFORM 3110-ASSESS-SUSPEND
               PERFORM 3120-FLAG-ANNUAL-FEE
               PERFORM 3130-EDIT-CONTACT
               PERFORM 3140-SPLIT-DRAW
               PERFORM 3150-FLAG-BALANCE
               PERFORM 3160-SPLIT-INTEREST
               PERFORM 3170-PRORATE-DRAW
               PERFORM 3180-ACCUM-TYPE
               PERFORM 3190-COUNT-RETRY
               PERFORM 3200-EDIT-NAME
               PERFORM 3210-COUNT-RETRY
               PERFORM 3220-ACCUM-CHANNEL
               PERFORM 3230-ACCUM-DRAW-PERIOD
               PERFORM 3240-ACCRUE-INTEREST
               PERFORM 3250-SPLIT-ANNUAL-FEE
               PERFORM 3260-ASSESS-PREFERRED
               PERFORM 3270-COUNT-RETRY
               PERFORM 3280-DESCRIBE-CHANNEL
               PERFORM 3290-SELECT-PENALTY-PCT
               PERFORM 3300-SPLIT-MIN-PAY
               PERFORM 3310-CHECK-NEXT-REVIEW
               PERFORM 3320-CHECK-MATURITY
               PERFORM 3330-ACCRUE-ANNUAL-FEE
               PERFORM 3340-FLAG-LIMIT
               PERFORM 3350-ASSESS-NOTICE-SENT
               PERFORM 3360-ACCUM-TYPE
               PERFORM 3370-ASSESS-OVERRIDE
               PERFORM 3380-DESCRIBE-CHANNEL
               PERFORM 3390-COUNT-EXCEPT
               PERFORM 3400-COUNT-SEQ
               PERFORM 3410-ACCRUE-ANNUAL-FEE
               PERFORM 3420-EDIT-OPERATOR
               PERFORM 3430-CHECK-EXPIRY
               PERFORM 3440-SPLIT-MIN-PAY
               PERFORM 3450-FLAG-INTEREST
               PERFORM 3460-CHECK-EFFECTIVE
               PERFORM 3470-AGE-MATURITY
               PERFORM 3480-CHECK-MATURITY
               PERFORM 3490-PRORATE-DRAW
               PERFORM 3500-LIMIT-ANNUAL-FEE
               PERFORM 3510-COUNT-SEQ
               PERFORM 3520-SPLIT-DRAW
               PERFORM 3530-EDIT-REF-NO
               PERFORM 3540-AGE-LAST-MAINT
               PERFORM 3550-CHECK-EFFECTIVE
               PERFORM 3560-SET-TRAN
               PERFORM 3570-COUNT-NOTICE
               PERFORM 3580-AGE-NEXT-DUE
               PERFORM 3590-AGE-POST
               PERFORM 3600-AGE-POST
               PERFORM 3610-EDIT-REF-NO
               PERFORM 3620-ASSESS-REVERSAL
               PERFORM 3630-SET-CHANNEL
               PERFORM 3640-ACCRUE-ANNUAL-FEE
               PERFORM 3650-COUNT-PAYMENT
               PERFORM 3660-SET-REGION
               PERFORM 3670-AGE-EXPIRY
               PERFORM 3680-ACCRUE-ANNUAL-FEE
               PERFORM 3690-SPLIT-DRAW
               PERFORM 3700-ASSESS-NOTICE-SENT
               PERFORM 3710-SET-LIEN-POS
               PERFORM 3720-LIMIT-APPRAISE
               PERFORM 3730-AGE-MATURITY
               PERFORM 3740-PRORATE-APPRAISE
               PERFORM 3750-LIMIT-MIN-PAY
               PERFORM 3760-FLAG-INTEREST
               PERFORM 3770-SELECT-MARGIN
               PERFORM 3780-PRORATE-MIN-PAY
               PERFORM 3790-ACCRUE-MIN-PAY
               PERFORM 3800-FLAG-INTEREST
               PERFORM 3810-ASSESS-PREFERRED
               PERFORM 3820-PRORATE-DRAW
               PERFORM 3830-SELECT-PENALTY-PCT
               PERFORM 3840-LIMIT-MIN-PAY
               PERFORM 3850-CHECK-NEXT-DUE
               PERFORM 3860-ACCUM-TYPE
               PERFORM 3870-EDIT-CUST-ID
               PERFORM 3880-ASSESS-PREFERRED
               PERFORM 3890-EDIT-REJECT
               PERFORM 3900-ASSESS-SUSPEND
               PERFORM 3910-ACCRUE-INTEREST
               PERFORM 3920-FLAG-APPRAISE
               PERFORM 3930-FLAG-INTEREST
               PERFORM 3940-ASSESS-AUTO-RENEW
               PERFORM 3950-ASSESS-HOLD
               PERFORM 3960-LIMIT-LIMIT
               PERFORM 3970-EDIT-ADDR
               PERFORM 3980-LIMIT-APPRAISE
               PERFORM 3990-ACCUM-STATUS
               PERFORM 4000-ACCUM-TRAN
               PERFORM 4010-CHECK-EXPIRY
               PERFORM 4020-CHECK-LAST-PAY
               PERFORM 4030-CHECK-NEXT-REVIEW
               PERFORM 4040-DESCRIBE-TYPE
               PERFORM 4050-SELECT-DISCOUNT
               PERFORM 4060-SELECT-MARGIN
               PERFORM 4070-EDIT-DESC
               PERFORM 4080-ACCRUE-APPRAISE
               PERFORM 4090-SELECT-MARGIN
               PERFORM 4100-DESCRIBE-DRAW-PERIOD
               PERFORM 4110-FLAG-INTEREST
               PERFORM 4120-COUNT-NOTICE
               PERFORM 4130-FLAG-APPRAISE
               PERFORM 4140-ACCUM-REASON
               PERFORM 4150-AGE-MATURITY
               PERFORM 4160-DESCRIBE-REASON
               PERFORM 4170-SPLIT-INTEREST
               PERFORM 4180-ACCRUE-APPRAISE
               PERFORM 4190-FLAG-LIMIT
               PERFORM 4200-SPLIT-LIMIT
               PERFORM 4210-ACCRUE-MIN-PAY
               PERFORM 4220-SET-REASON
               PERFORM 7020-CALL-HESTAT
               REWRITE HEL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'HELMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM HEL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF HELT-POST-DATE > 0 AND HELT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = HELT-LIMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELT-LIMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE HELT-DRAW-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE MIN PAY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3030-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-MIN-PAY-AMT
                       * HELM-DISCOUNT / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 12.00 TO HELM-INTEREST-YTD
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 15.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-MIN-PAY-AMT
                       * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 10.00
               MOVE 10.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 50.00
               MOVE 50.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E455 DESC REQUIRED
      * E578 BALANCE AMT NEGATIVE
      * E257 BALANCE AMT OVER 25000
      * E783 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-AMT < 0
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-AMT > 25000.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN HELT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3050-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-INTEREST-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-INTEREST-AMT
           END-IF
           ADD HELM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3060-FLAG-BALANCE.
           IF HELM-BALANCE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY GRID TIER FOR THE ANNUAL FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-GRID-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF HELM-ANNUAL-FEE-YTD NOT < WS-MIN-PAY-GRID-LOW(WS-TX)
                   MOVE WS-MIN-PAY-GRID-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 100.00
               MOVE 100.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 5000.00
               MOVE 5000.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E395 REF NO REQUIRED
      * E454 LIMIT AMT NEGATIVE
      * E237 LIMIT AMT OVER 25000
      * E874 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT < 0
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT > 25000.00
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN HELM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3100-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELT-APPRAISE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELT-APPRAISE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE HELM-DRAW-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE DRAW YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3110-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-DRAW-YTD *
                       HELM-MARGIN / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 10.00 TO HELM-INTEREST-YTD
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 5.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-DRAW-YTD *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 5.00
               MOVE 5.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 150.00
               MOVE 150.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3120-FLAG-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELT-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E377 CONTACT REQUIRED
      * E680 ANNUAL FEE AMT NEGATIVE
      * E356 ANNUAL FEE AMT OVER 25000
      * E474 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CONTACT = SPACES
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN HELT-ANNUAL-FEE-AMT < 0
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN HELT-ANNUAL-FEE-AMT > 25000.00
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN HELM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E474' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELM-DRAW-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELM-DRAW-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3150-FLAG-BALANCE.
           IF HELM-BALANCE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HELM-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELM-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRAW AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-DRAW-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-DRAW-AMT
           END-IF
           ADD HELM-DRAW-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 5
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E097 NAME REQUIRED
      * E709 LIMIT AMT NEGATIVE
      * E310 LIMIT AMT OVER 99999
      * E909 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-NAME = SPACES
                   MOVE 'E097' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT < 0
                   MOVE 'E709' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT > 99999.00
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN HELM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 6
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-CHANNEL-CD
                   ADD HELM-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY DRAW PERIOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3230-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-DRAW-PERIOD-CD
                   ADD HELM-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-INTEREST.
           IF HELM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-INTEREST-YTD * HELM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HELM-APPRAISE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HELM-ANNUAL-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELM-ANNUAL-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE HELM-MIN-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE ANNUAL FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3260-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELT-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELT-LIMIT-AMT ROUNDED = HELM-ANNUAL-FEE-YTD
                       * HELM-DISCOUNT / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 12.00 TO HELT-LIMIT-AMT
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 10.00 TO HELT-LIMIT-AMT
               WHEN OTHER
                   COMPUTE HELT-LIMIT-AMT ROUNDED = HELM-ANNUAL-FEE-YTD
                       * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELT-LIMIT-AMT > 0 AND HELT-LIMIT-AMT < 2.00
               MOVE 2.00 TO HELT-LIMIT-AMT
           END-IF
           IF HELT-LIMIT-AMT > 25.00
               MOVE 25.00 TO HELT-LIMIT-AMT
           END-IF
           ADD HELT-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3270-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 12
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELT-ANNUAL-FEE-AMT NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN PAY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-MIN-PAY.
           COMPUTE WS-WORK-AMT = HELM-MIN-PAY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELM-MIN-PAY-AMT - (WS-WORK-AMT * 12)
           COMPUTE HELM-DRAW-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-YTD * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELM-MIN-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3340-FLAG-LIMIT.
           IF HELT-LIMIT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE MIN PAY YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3350-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-AMT ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-PENALTY-PCT / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 20.00 TO HELM-BALANCE-AMT
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 5.00 TO HELM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE HELM-BALANCE-AMT ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-BALANCE-AMT > 0 AND HELM-BALANCE-AMT < 10.00
               MOVE 10.00 TO HELM-BALANCE-AMT
           END-IF
           IF HELM-BALANCE-AMT > 100.00
               MOVE 100.00 TO HELM-BALANCE-AMT
           END-IF
           ADD HELM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-TYPE-CD
                   ADD HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE YTD FEE ON THE MIN PAY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3370-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-MIN-PAY-YTD
                       * HELM-DISCOUNT / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 10.00 TO HELM-APPRAISE-YTD
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 20.00 TO HELM-APPRAISE-YTD
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-MIN-PAY-YTD
                       * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-APPRAISE-YTD > 0 AND HELM-APPRAISE-YTD < 5.00
               MOVE 5.00 TO HELM-APPRAISE-YTD
           END-IF
           IF HELM-APPRAISE-YTD > 100.00
               MOVE 100.00 TO HELM-APPRAISE-YTD
           END-IF
           ADD HELM-APPRAISE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 6
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-AMT * HELM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO HELM-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E911 OPERATOR REQUIRED
      * E413 LIMIT AMT NEGATIVE
      * E796 LIMIT AMT OVER 1000000
      * E193 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-OPERATOR = SPACES
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT < 0
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT > 1000000.00
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN HELM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E193' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN PAY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-MIN-PAY.
           COMPUTE WS-WORK-AMT = HELM-MIN-PAY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELM-MIN-PAY-AMT - (WS-WORK-AMT * 12)
           COMPUTE HELT-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3450-FLAG-INTEREST.
           IF HELM-INTEREST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRAW YTD.
      *----------------------------------------------------------------*
       3490-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-DRAW-YTD ROUNDED =
                   HELM-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-DRAW-YTD
           END-IF
           ADD HELM-DRAW-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 50.00
               MOVE 50.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 1000.00
               MOVE 1000.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 3
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELM-DRAW-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELM-DRAW-YTD - (WS-WORK-AMT * 12)
           COMPUTE HELM-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E039 REF NO REQUIRED
      * E995 MIN PAY AMT NEGATIVE
      * E784 MIN PAY AMT OVER 5000
      * E374 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E039' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT < 0
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT > 5000.00
                   MOVE 'E784' TO WS-REJECT-CODE
               WHEN HELT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN EFF DATE IS PAST
      * PY -> FZ WHEN EFF DATE IS PAST
      * FZ -> RC WHEN EFF DATE IS PAST
      * RC -> AF WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF HELT-POST-DATE > 0 AND HELT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF HELT-POST-DATE > 0 AND HELT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E594 REF NO REQUIRED
      * E294 MIN PAY AMT NEGATIVE
      * E169 MIN PAY AMT OVER 250000
      * E248 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT < 0
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT > 250000.00
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN HELM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW YTD FEE ON THE APPRAISE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3620-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO HELM-DRAW-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE HELM-DRAW-YTD ROUNDED = HELT-APPRAISE-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 7.00 TO HELM-DRAW-YTD
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 10.00 TO HELM-DRAW-YTD
               WHEN OTHER
                   COMPUTE HELM-DRAW-YTD ROUNDED = HELT-APPRAISE-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-DRAW-YTD > 0 AND HELM-DRAW-YTD < 1.00
               MOVE 1.00 TO HELM-DRAW-YTD
           END-IF
           IF HELM-DRAW-YTD > 25.00
               MOVE 25.00 TO HELM-DRAW-YTD
           END-IF
           ADD HELM-DRAW-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-AMT * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN MATURITY DATE IS PAST
      * HKW -> HPB WHEN MATURITY DATE IS PAST
      * HPB -> ULJ WHEN MATURITY DATE IS PAST
      * ULJ -> BSD WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-YTD * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELT-DRAW-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELT-DRAW-AMT - (WS-WORK-AMT * 12)
           COMPUTE HELM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE DRAW YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3700-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-DRAW-YTD *
                       HELM-DISCOUNT / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 20.00 TO HELM-INTEREST-YTD
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 12.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-DRAW-YTD *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 2.00
               MOVE 2.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 35.00
               MOVE 35.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN LAST PAY DATE IS PAST
      * PVA -> SFC WHEN LAST PAY DATE IS PAST
      * SFC -> AXX WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * APPRAISE YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-APPRAISE.
           IF HELM-APPRAISE-YTD < 10.00
               MOVE 10.00 TO HELM-APPRAISE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-YTD > 10000.00
               MOVE 10000.00 TO HELM-APPRAISE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       3740-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-AMT < 50.00
               MOVE 50.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-AMT > 5000.00
               MOVE 5000.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3760-FLAG-INTEREST.
           IF HELM-INTEREST-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELM-BALANCE-AMT NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN PAY AMT.
      *----------------------------------------------------------------*
       3780-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-MIN-PAY-AMT ROUNDED =
                   HELM-LIMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-MIN-PAY-AMT
           END-IF
           ADD HELM-MIN-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-AMT * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELM-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-INTEREST.
           IF HELM-INTEREST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3810-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-AMT ROUNDED =
                       HELT-ANNUAL-FEE-AMT * HELM-PENALTY-PCT / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 20.00 TO HELM-BALANCE-AMT
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 7.00 TO HELM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE HELM-BALANCE-AMT ROUNDED =
                       HELT-ANNUAL-FEE-AMT * HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-BALANCE-AMT > 0 AND HELM-BALANCE-AMT < 1.00
               MOVE 1.00 TO HELM-BALANCE-AMT
           END-IF
           IF HELM-BALANCE-AMT > 75.00
               MOVE 75.00 TO HELM-BALANCE-AMT
           END-IF
           ADD HELM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRAW AMT.
      *----------------------------------------------------------------*
       3820-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-DRAW-AMT ROUNDED =
                   HELM-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELT-DRAW-AMT
           END-IF
           ADD HELT-DRAW-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIERX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIERX-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-MIN-PAY-TIERX-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIERX-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3840-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-AMT < 50.00
               MOVE 50.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-AMT > 500.00
               MOVE 500.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E250 CUST ID REQUIRED
      * E864 MIN PAY YTD NEGATIVE
      * E457 MIN PAY YTD OVER 50000
      * E661 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CUST-ID = SPACES
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD < 0
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD > 50000.00
                   MOVE 'E457' TO WS-REJECT-CODE
               WHEN HELM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE MIN PAY YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3880-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                       HELM-MIN-PAY-YTD * HELM-MARGIN / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO HELM-ANNUAL-FEE-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO HELM-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                       HELM-MIN-PAY-YTD * HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-ANNUAL-FEE-AMT > 0 AND HELM-ANNUAL-FEE-AMT < 1.00
               MOVE 1.00 TO HELM-ANNUAL-FEE-AMT
           END-IF
           IF HELM-ANNUAL-FEE-AMT > 25.00
               MOVE 25.00 TO HELM-ANNUAL-FEE-AMT
           END-IF
           ADD HELM-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E403 REJECT CD REQUIRED
      * E617 BALANCE YTD NEGATIVE
      * E423 BALANCE YTD OVER 1000000
      * E349 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-REJECT-CD = SPACES
                   MOVE 'E403' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD < 0
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD > 1000000.00
                   MOVE 'E423' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE MIN PAY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3900-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-DISCOUNT / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-INTEREST.
           IF HELM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-INTEREST-AMT * HELM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3920-FLAG-APPRAISE.
           IF HELM-APPRAISE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3930-FLAG-INTEREST.
           IF HELM-INTEREST-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE MIN PAY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3940-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELT-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELT-LIMIT-AMT ROUNDED = HELM-MIN-PAY-AMT *
                       HELM-MARGIN / 200
               WHEN HELM-REGION-CD = 'BSD'
                   MOVE 3.00 TO HELT-LIMIT-AMT
               WHEN HELM-REGION-CD = 'HKW'
                   MOVE 7.00 TO HELT-LIMIT-AMT
               WHEN OTHER
                   COMPUTE HELT-LIMIT-AMT ROUNDED = HELM-MIN-PAY-AMT *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELT-LIMIT-AMT > 0 AND HELT-LIMIT-AMT < 1.00
               MOVE 1.00 TO HELT-LIMIT-AMT
           END-IF
           IF HELT-LIMIT-AMT > 250.00
               MOVE 250.00 TO HELT-LIMIT-AMT
           END-IF
           ADD HELT-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3950-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HELM-LIMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE HELM-LIMIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       HELM-MARGIN / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 15.00 TO HELM-LIMIT-YTD
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 12.00 TO HELM-LIMIT-YTD
               WHEN OTHER
                   COMPUTE HELM-LIMIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-LIMIT-YTD > 0 AND HELM-LIMIT-YTD < 10.00
               MOVE 10.00 TO HELM-LIMIT-YTD
           END-IF
           IF HELM-LIMIT-YTD > 75.00
               MOVE 75.00 TO HELM-LIMIT-YTD
           END-IF
           ADD HELM-LIMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3960-LIMIT-LIMIT.
           IF HELM-LIMIT-AMT < 25.00
               MOVE 25.00 TO HELM-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-LIMIT-AMT > 500.00
               MOVE 500.00 TO HELM-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E893 ADDR REQUIRED
      * E243 APPRAISE AMT NEGATIVE
      * E264 APPRAISE AMT OVER 99999
      * E824 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-ADDR = SPACES
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN HELT-APPRAISE-AMT < 0
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN HELT-APPRAISE-AMT > 99999.00
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN HELM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-APPRAISE.
           IF HELM-APPRAISE-AMT < 10.00
               MOVE 10.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-AMT > 500.00
               MOVE 500.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-STATUS-CD
                   ADD HELM-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DRAW YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-TRAN-CD
                   ADD HELM-DRAW-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-DRAW-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY GRID TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-GRID-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF HELM-MIN-PAY-YTD NOT < WS-MIN-PAY-GRID-LOW(WS-TX)
                   MOVE WS-MIN-PAY-GRID-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF HELM-LIMIT-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E378 DESC REQUIRED
      * E179 WORK AMT NEGATIVE
      * E951 WORK AMT OVER 50000
      * E878 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E951' TO WS-REJECT-CODE
               WHEN HELM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON APPRAISE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-APPRAISE.
           IF HELM-APPRAISE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-APPRAISE-YTD * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELT-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIERX TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIERX-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-INTEREST-YTD NOT < WS-MIN-PAY-TIERX-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIERX-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4110-FLAG-INTEREST.
           IF HELM-INTEREST-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4130-FLAG-APPRAISE.
           IF HELM-APPRAISE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REASON-CD
                   ADD HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4150-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4160-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HELM-INTEREST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELM-INTEREST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE HELM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON APPRAISE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-APPRAISE.
           IF HELT-APPRAISE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELT-APPRAISE-AMT * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELM-APPRAISE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4190-FLAG-LIMIT.
           IF HELM-LIMIT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = HELT-LIMIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELT-LIMIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-MIN-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4210-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-YTD * HELM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN LAST MAINT DATE IS PAST
      * NWS -> ZJM WHEN LAST MAINT DATE IS PAST
      * ZJM -> KST WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4220-SET-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KST' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE HEL FIELDS TO HESTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-HESTAT.
           INITIALIZE HESTAT-PARMS
           MOVE HELM-DRAW-AMT
             TO HESTAT-DRAW-AMT
           MOVE HELM-ANNUAL-FEE-AMT
             TO HESTAT-ANNUAL-FEE-AMT
           MOVE HELM-APPRAISE-YTD
             TO HESTAT-APPRAISE-YTD
           MOVE HELM-BALANCE-AMT
             TO HESTAT-BALANCE-AMT
           MOVE HELM-ANNUAL-FEE-YTD
             TO HESTAT-ANNUAL-FEE-YTD
           MOVE HELM-BALANCE-YTD
             TO HESTAT-BALANCE-YTD
           MOVE HELM-EFFECTIVE-DATE
             TO HESTAT-EFFECTIVE-DATE
           MOVE HELM-OPEN-DATE
             TO HESTAT-OPEN-DATE
           MOVE HELM-LAST-MAINT-DATE
             TO HESTAT-LAST-MAINT-DATE
           MOVE HELM-CLOSE-DATE
             TO HESTAT-CLOSE-DATE
           MOVE HELM-REASON-CD
             TO HESTAT-REASON-CD
           MOVE HELM-TYPE-CD
             TO HESTAT-TYPE-CD
           MOVE HELM-REGION-CD
             TO HESTAT-REGION-CD
           MOVE HELM-DRAW-PERIOD-CD
             TO HESTAT-DRAW-PERIOD-CD
           MOVE HELM-PREFERRED-FLAG
             TO HESTAT-PREFERRED-FLAG
           MOVE HELM-AUTO-RENEW-FLAG
             TO HESTAT-AUTO-RENEW-FLAG
           MOVE HELM-OVERRIDE-FLAG
             TO HESTAT-OVERRIDE-FLAG
           MOVE HELM-RETRY-CNT
             TO HESTAT-RETRY-CNT
           MOVE HELM-PAYMENT-CNT
             TO HESTAT-PAYMENT-CNT
           MOVE HELM-DISCOUNT
             TO HESTAT-DISCOUNT
           MOVE HELM-MARGIN
             TO HESTAT-MARGIN
           MOVE HELM-CONTACT
             TO HESTAT-CONTACT
           MOVE HELM-CUST-ID
             TO HESTAT-CUST-ID
           CALL 'HESTAT' USING HESTAT-PARMS
           EVALUATE HESTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE HESTAT-DRAW-AMT
                     TO HELM-DRAW-AMT
                   MOVE HESTAT-ANNUAL-FEE-AMT
                     TO HELM-ANNUAL-FEE-AMT
                   MOVE HESTAT-BALANCE-AMT
                     TO HELM-BALANCE-AMT
                   MOVE HESTAT-PREFERRED-FLAG
                     TO HELM-PREFERRED-FLAG
                   MOVE HESTAT-OVERRIDE-FLAG
                     TO HELM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-HESTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE HELT-KEY TO RD-KEY
           MOVE HELT-TRAN-CD TO RD-CODE
           MOVE HELT-DRAW-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE HELT-KEY TO RD-KEY
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
           DISPLAY 'HELNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'HELNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
