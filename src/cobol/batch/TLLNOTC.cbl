      *================================================================*
      * PROGRAM  : TLLNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELLER BALANCING - CUSTOMER NOTICES. READS THE TBL  *
      *            TRANSACTIONS, UPDATES THE TBL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TELLER CASH DRAWER BALANCING,   *
      *            BATCH PROOF AND OVER/SHORT                          *
      * INPUT    : TBLMST (KSDS), TBLTRN, BANKCTL                      *
      * OUTPUT   : TBLOUT, TLLNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TLFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-06-22 JLM  ORIGINAL                                       *
      * 1999-08-16 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-03-06 LMB  CASH-IN EXCEPTIONS TO OPS QUEUE REQ0897        *
      * 2004-07-07 CRP  VAULT-BUY TABLE EXPANDED INC8205               *
      * 2005-05-06 CRP  OVERSHORT FEE SCHEDULE UPDATE PRB2142          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLLNOTC.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TBLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TBLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TBLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TBLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TLLNORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TBLMST.
       FD  TRAN-FILE.
       COPY TBLTRN.
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
      * WS-CASH-OUT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000320500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000231000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000498500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000654500'.
       01  WS-CASH-OUT-BAND-TBL REDEFINES WS-CASH-OUT-BAND-VALUES.
           05  WS-CASH-OUT-BAND-ROW OCCURS 4 TIMES.
               10  WS-CASH-OUT-BAND-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-OUT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000171000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000408000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000770500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000449500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000184500'.
       01  WS-CASH-OUT-TIER-TBL REDEFINES WS-CASH-OUT-TIER-VALUES.
           05  WS-CASH-OUT-TIER-ROW OCCURS 5 TIMES.
               10  WS-CASH-OUT-TIER-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-OVERSHOR-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OVERSHOR-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000340000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000658000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000476500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000060000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010400000000035500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010900000000361500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011900000000029500'.
       01  WS-OVERSHOR-GRID-TBL REDEFINES WS-OVERSHOR-GRID-VALUES.
           05  WS-OVERSHOR-GRID-ROW OCCURS 7 TIMES.
               10  WS-OVERSHOR-GRID-LOW    PIC 9(09)V99.
               10  WS-OVERSHOR-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TLLNOTC  TELLER BALANCING'.
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
       COPY TLFEEP.

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
               MOVE 'TBLMST' TO ABND-FILE-NAME
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
           MOVE TBLT-KEY TO TBLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-OVERSHORT
               PERFORM 3010-ACCRUE-RATE
               PERFORM 3020-COUNT-ITEM
               PERFORM 3030-ACCUM-CHANNEL
               PERFORM 3040-COUNT-RETRY
               PERFORM 3050-SELECT-FEE-PCT
               PERFORM 3060-ASSESS-HOLD
               PERFORM 3070-PRORATE-VAULT-BUY
               PERFORM 3080-EDIT-OPERATOR
               PERFORM 3090-PRORATE-CASH-IN
               PERFORM 3100-SPLIT-VAULT-SELL
               PERFORM 3110-SET-CHANNEL
               PERFORM 3120-PRORATE-OVERSHORT
               PERFORM 3130-AGE-EXPIRY
               PERFORM 3140-FLAG-VAULT-BUY
               PERFORM 3150-SPLIT-VAULT-SELL
               PERFORM 3160-SET-PROOF-TYPE
               PERFORM 3170-SPLIT-VAULT-BUY
               PERFORM 3180-ACCUM-SOURCE
               PERFORM 3190-ASSESS-EXEMPT
               PERFORM 3200-COUNT-SEQ
               PERFORM 3210-COUNT-EXCEPT
               PERFORM 3220-AGE-POST
               PERFORM 3230-CHECK-EFFECTIVE
               PERFORM 3240-PRORATE-CASH-OUT
               PERFORM 3250-ASSESS-HOLD
               PERFORM 3260-CHECK-OPEN
               PERFORM 3270-EDIT-DESC
               PERFORM 3280-SET-SOURCE
               PERFORM 3290-SELECT-RATE
               PERFORM 3300-ACCUM-DRAWER
               PERFORM 3310-ASSESS-EXEMPT
               PERFORM 3320-AGE-LAST-STMT
               PERFORM 3330-ACCRUE-BATCH
               PERFORM 3340-ACCRUE-BATCH
               PERFORM 3350-AGE-EFF
               PERFORM 3360-EDIT-REF-NO
               PERFORM 3370-ACCUM-DRAWER
               PERFORM 3380-FLAG-CASH-OUT
               PERFORM 3390-PRORATE-CASH-OUT
               PERFORM 3400-SET-SOURCE
               PERFORM 3410-PRORATE-CASH-OUT
               PERFORM 3420-SPLIT-BATCH
               PERFORM 3430-AGE-NOTICE
               PERFORM 3440-FLAG-BATCH
               PERFORM 3450-SET-CHANNEL
               PERFORM 3460-EDIT-ADDR
               PERFORM 3470-LIMIT-CASH-IN
               PERFORM 3480-CHECK-NOTICE
               PERFORM 3490-ACCUM-REASON
               PERFORM 3500-COUNT-EXCEPT
               PERFORM 3510-SET-CHANNEL
               PERFORM 3520-LIMIT-BATCH
               PERFORM 3530-ACCUM-CHANNEL
               PERFORM 3540-AGE-LAST-ACT
               PERFORM 3550-COUNT-RETRY
               PERFORM 3560-SPLIT-VAULT-SELL
               PERFORM 3570-ASSESS-REVIEW
               PERFORM 3580-CHECK-EXPIRY
               PERFORM 3590-AGE-LAST-ACT
               PERFORM 3600-PRORATE-VAULT-SELL
               PERFORM 3610-LIMIT-VAULT-BUY
               PERFORM 3620-DESCRIBE-TRAN
               PERFORM 3630-FLAG-BATCH
               PERFORM 3640-SELECT-RATE
               PERFORM 3650-LIMIT-CASH-OUT
               PERFORM 3660-ACCUM-CHANNEL
               PERFORM 3670-PRORATE-VAULT-BUY
               PERFORM 3680-ACCUM-SOURCE
               PERFORM 3690-SET-PROOF-TYPE
               PERFORM 3700-SPLIT-VAULT-SELL
               PERFORM 3710-EDIT-DESC
               PERFORM 3720-ASSESS-OVERRIDE
               PERFORM 3730-FLAG-BATCH
               PERFORM 3740-COUNT-EXCEPT
               PERFORM 3750-SPLIT-VAULT-SELL
               PERFORM 3760-LIMIT-ITEM
               PERFORM 3770-EDIT-REJECT
               PERFORM 3780-PRORATE-CASH-IN
               PERFORM 3790-SPLIT-CASH-IN
               PERFORM 3800-FLAG-BATCH
               PERFORM 3810-DESCRIBE-CYCLE
               PERFORM 3820-LIMIT-VAULT-BUY
               PERFORM 3830-EDIT-REJECT
               PERFORM 3840-AGE-EFFECTIVE
               PERFORM 3850-SET-REASON
               PERFORM 3860-DESCRIBE-CHANNEL
               PERFORM 3870-PRORATE-VAULT-BUY
               PERFORM 3880-CHECK-LAST-STMT
               PERFORM 3890-ACCUM-SOURCE
               PERFORM 3900-DESCRIBE-CHANNEL
               PERFORM 3910-SET-CHANNEL
               PERFORM 3920-SELECT-RATE
               PERFORM 3930-ACCRUE-VAULT-SELL
               PERFORM 3940-PRORATE-VAULT-BUY
               PERFORM 3950-COUNT-ITEM
               PERFORM 3960-SET-PROOF-TYPE
               PERFORM 3970-SET-CYCLE
               PERFORM 3980-SPLIT-CASH-OUT
               PERFORM 3990-DESCRIBE-REASON
               PERFORM 4000-DESCRIBE-PROOF-TYPE
               PERFORM 4010-ASSESS-OVERRIDE
               PERFORM 4020-SET-DRAWER
               PERFORM 4030-ACCRUE-OVERSHORT
               PERFORM 4040-SELECT-FEE-PCT
               PERFORM 4050-SPLIT-BATCH
               PERFORM 4060-ACCRUE-CASH-OUT
               PERFORM 7000-CALL-TLFEE
               REWRITE TBL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TBLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TBL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-OVERSHORT.
           IF TBLT-OVERSHORT-AMT < 0.00
               MOVE 0.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-OVERSHORT-AMT > 5000.00
               MOVE 5000.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT TIER TIER FOR THE OVERSHORT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-TIER-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-OVERSHORT-YTD NOT < WS-CASH-OUT-TIER-LOW(WS-TX)
                   MOVE WS-CASH-OUT-TIER-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE BATCH AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3060-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-BATCH-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 10.00 TO TBLM-CASH-IN-YTD
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 12.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-BATCH-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 2.00
               MOVE 2.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 35.00
               MOVE 35.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3070-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 OPERATOR REQUIRED
      * E305 BATCH YTD NEGATIVE
      * E003 BATCH YTD OVER 5000
      * E519 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD < 0
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD > 5000.00
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN TBLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH IN YTD.
      *----------------------------------------------------------------*
       3090-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3100-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TBLT-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN EFFECTIVE DATE IS PAST
      * F -> M WHEN EFFECTIVE DATE IS PAST
      * M -> N WHEN EFFECTIVE DATE IS PAST
      * N -> R WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3140-FLAG-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-BUY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN NOTICE DATE IS PAST
      * RX -> TW WHEN NOTICE DATE IS PAST
      * TW -> UG WHEN NOTICE DATE IS PAST
      * UG -> XY WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RX' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TW' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XY' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-BUY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-BUY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TBLT-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-SOURCE-CD
                   ADD TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3190-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-HOLD-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-DISCOUNT / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 5.00 TO TBLM-OVERSHORT-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 7.00 TO TBLM-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-AMT > 0 AND TBLM-OVERSHORT-AMT < 5.00
               MOVE 5.00 TO TBLM-OVERSHORT-AMT
           END-IF
           IF TBLM-OVERSHORT-AMT > 35.00
               MOVE 35.00 TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 10
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BATCH YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                   TBLM-BATCH-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-OUT-AMT
           END-IF
           ADD TBLM-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3250-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TBLT-OVERSHORT-AMT *
                       TBLM-FEE-PCT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TBLT-OVERSHORT-AMT *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3260-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E094 DESC REQUIRED
      * E001 VAULT SELL AMT NEGATIVE
      * E237 VAULT SELL AMT OVER 10000
      * E949 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN TBLT-VAULT-SELL-AMT < 0
                   MOVE 'E001' TO WS-REJECT-CODE
               WHEN TBLT-VAULT-SELL-AMT > 10000.00
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN TBLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN POST DATE IS PAST
      * PT -> SZ WHEN POST DATE IS PAST
      * SZ -> VK WHEN POST DATE IS PAST
      * VK -> VW WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT BAND TIER FOR THE CASH OUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-BAND-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-CASH-OUT-AMT NOT < WS-CASH-OUT-BAND-LOW(WS-TX)
                   MOVE WS-CASH-OUT-BAND-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-DRAWER-CD
                   ADD TBLT-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE CASH OUT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3310-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-HOLD-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-DISCOUNT / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 20.00 TO TBLM-VAULT-SELL-AMT
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 5.00 TO TBLM-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-VAULT-SELL-AMT > 0 AND TBLM-VAULT-SELL-AMT < 1.00
               MOVE 1.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 50.00
               MOVE 50.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-BATCH.
           IF TBLM-BATCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-YTD * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-BATCH.
           IF TBLT-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-BATCH-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-VAULT-BUY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E826 REF NO REQUIRED
      * E369 WORK AMT NEGATIVE
      * E777 WORK AMT OVER 10000
      * E767 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-REF-NO = SPACES
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT YTD BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-DRAWER-CD
                   ADD TBLM-CASH-OUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-OUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3380-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3390-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN OPEN DATE IS PAST
      * PT -> SZ WHEN OPEN DATE IS PAST
      * SZ -> VK WHEN OPEN DATE IS PAST
      * VK -> VW WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3410-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-CASH-OUT-AMT
           END-IF
           ADD TBLM-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-YTD - (WS-WORK-AMT * 12)
           COMPUTE TBLM-VAULT-SELL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3440-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E845 ADDR REQUIRED
      * E308 VAULT BUY YTD NEGATIVE
      * E014 VAULT BUY YTD OVER 1000000
      * E600 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E845' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-YTD < 0
                   MOVE 'E308' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-YTD > 1000000.00
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN TBLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-YTD < 10.00
               MOVE 10.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-YTD > 2500.00
               MOVE 2500.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BATCH YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-BATCH.
           IF TBLM-BATCH-YTD < 50.00
               MOVE 50.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-YTD > 50000.00
               MOVE 50000.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TBLM-CASH-IN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE OVERSHORT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3570-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TBLM-OVERSHORT-YTD *
                       TBLM-FEE-PCT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TBLM-OVERSHORT-YTD *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       3600-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VAULT BUY YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3610-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD < 0.00
               MOVE 0.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-YTD > 500.00
               MOVE 500.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BO'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'OS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3630-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT BAND TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-BAND-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-CASH-IN-AMT NOT < WS-CASH-OUT-BAND-LOW(WS-TX)
                   MOVE WS-CASH-OUT-BAND-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3650-LIMIT-CASH-OUT.
           IF TBLT-CASH-OUT-AMT < 50.00
               MOVE 50.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-CASH-OUT-AMT > 500.00
               MOVE 500.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLM-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT BUY AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-BUY-AMT ROUNDED =
                   TBLT-CASH-OUT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-BUY-AMT
           END-IF
           ADD TBLM-VAULT-BUY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-SOURCE-CD
                   ADD TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN OPEN DATE IS PAST
      * RX -> TW WHEN OPEN DATE IS PAST
      * TW -> UG WHEN OPEN DATE IS PAST
      * UG -> XY WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RX' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TW' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XY' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E377 DESC REQUIRED
      * E904 CASH IN AMT NEGATIVE
      * E987 CASH IN AMT OVER 5000
      * E096 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 5000.00
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN TBLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE CASH OUT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3720-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-CASH-OUT-YTD
                       * TBLM-FEE-PCT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 10.00 TO TBLM-CASH-IN-YTD
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 3.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-CASH-OUT-YTD
                       * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 5.00
               MOVE 5.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 25.00
               MOVE 25.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3750-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TBLM-VAULT-BUY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E740 REJECT CD REQUIRED
      * E088 VAULT SELL YTD NEGATIVE
      * E335 VAULT SELL YTD OVER 10000
      * E947 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD < 0
                   MOVE 'E088' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD > 10000.00
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN TBLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E947' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       3780-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CASH IN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TBLM-CASH-IN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-IN-YTD - (WS-WORK-AMT * 3)
           COMPUTE TBLM-VAULT-BUY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3820-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD < 100.00
               MOVE 100.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-YTD > 1000.00
               MOVE 1000.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E742 REJECT CD REQUIRED
      * E962 CASH OUT AMT NEGATIVE
      * E156 CASH OUT AMT OVER 25000
      * E044 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN TBLT-CASH-OUT-AMT < 0
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN TBLT-CASH-OUT-AMT > 25000.00
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN NEXT REVIEW DATE IS PAST
      * GV -> PG WHEN NEXT REVIEW DATE IS PAST
      * PG -> SX WHEN NEXT REVIEW DATE IS PAST
      * SX -> XN WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GV' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PG' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SX' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SX'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XN' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT BUY YTD.
      *----------------------------------------------------------------*
       3870-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-BUY-YTD ROUNDED =
                   TBLM-VAULT-SELL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-VAULT-BUY-YTD
           END-IF
           ADD TBLM-VAULT-BUY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-SOURCE-CD
                   ADD TBLM-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT TIER TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-TIER-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-CASH-OUT-YTD NOT < WS-CASH-OUT-TIER-LOW(WS-TX)
                   MOVE WS-CASH-OUT-TIER-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLM-VAULT-BUY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT BUY YTD.
      *----------------------------------------------------------------*
       3940-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-BUY-YTD ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-VAULT-BUY-YTD
           END-IF
           ADD TBLM-VAULT-BUY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN LAST ACT DATE IS PAST
      * RX -> TW WHEN LAST ACT DATE IS PAST
      * TW -> UG WHEN LAST ACT DATE IS PAST
      * UG -> XY WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RX' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TW' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XY' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN NEXT REVIEW DATE IS PAST
      * TA -> TG WHEN NEXT REVIEW DATE IS PAST
      * TG -> WG WHEN NEXT REVIEW DATE IS PAST
      * WG -> KX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE TBLM-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PROOF TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE VAULT BUY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-AMT ROUNDED = TBLM-VAULT-BUY-YTD *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 7.00 TO TBLM-BATCH-AMT
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 7.00 TO TBLM-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLM-BATCH-AMT ROUNDED = TBLM-VAULT-BUY-YTD *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-BATCH-AMT > 0 AND TBLM-BATCH-AMT < 10.00
               MOVE 10.00 TO TBLM-BATCH-AMT
           END-IF
           IF TBLM-BATCH-AMT > 100.00
               MOVE 100.00 TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAWER CD:
      * D -> F WHEN LAST STMT DATE IS PAST
      * F -> R WHEN LAST STMT DATE IS PAST
      * R -> D WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR GRID TIER FOR THE OVERSHORT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-GRID-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TBLM-OVERSHORT-YTD NOT < WS-OVERSHOR-GRID-LOW(WS-TX)
                   MOVE WS-OVERSHOR-GRID-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4050-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-OVERSHORT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-CASH-OUT.
           IF TBLM-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-OUT-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-VAULT-SELL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TLFEE.
           INITIALIZE TLFEE-PARMS
           MOVE TBLM-VAULT-BUY-AMT
             TO TLFEE-VAULT-BUY-AMT
           MOVE TBLM-VAULT-SELL-YTD
             TO TLFEE-VAULT-SELL-YTD
           MOVE TBLM-CASH-IN-YTD
             TO TLFEE-CASH-IN-YTD
           MOVE TBLM-VAULT-BUY-YTD
             TO TLFEE-VAULT-BUY-YTD
           MOVE TBLM-VAULT-SELL-AMT
             TO TLFEE-VAULT-SELL-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLFEE-CASH-OUT-YTD
           MOVE TBLM-OPEN-DATE
             TO TLFEE-OPEN-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLFEE-NEXT-REVIEW-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLFEE-LAST-PAY-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLFEE-LAST-ACT-DATE
           MOVE TBLM-CHANNEL-CD
             TO TLFEE-CHANNEL-CD
           MOVE TBLM-PROOF-TYPE-CD
             TO TLFEE-PROOF-TYPE-CD
           MOVE TBLM-DRAWER-CD
             TO TLFEE-DRAWER-CD
           MOVE TBLM-CYCLE-CD
             TO TLFEE-CYCLE-CD
           MOVE TBLM-EXEMPT-FLAG
             TO TLFEE-EXEMPT-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLFEE-STOP-FLAG
           MOVE TBLM-OVERRIDE-FLAG
             TO TLFEE-OVERRIDE-FLAG
           MOVE TBLM-EXCEPT-CNT
             TO TLFEE-EXCEPT-CNT
           MOVE TBLM-NOTICE-CNT
             TO TLFEE-NOTICE-CNT
           MOVE TBLM-DISCOUNT
             TO TLFEE-DISCOUNT
           MOVE TBLM-RATE
             TO TLFEE-RATE
           MOVE TBLM-DESC
             TO TLFEE-DESC
           MOVE TBLM-NAME
             TO TLFEE-NAME
           CALL 'TLFEE' USING TLFEE-PARMS
           EVALUATE TLFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLFEE-VAULT-SELL-YTD
                     TO TBLM-VAULT-SELL-YTD
                   MOVE TLFEE-CASH-IN-YTD
                     TO TBLM-CASH-IN-YTD
                   MOVE TLFEE-CASH-OUT-YTD
                     TO TBLM-CASH-OUT-YTD
                   MOVE TLFEE-STOP-FLAG
                     TO TBLM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TLFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TBLT-KEY TO RD-KEY
           MOVE TBLT-TRAN-CD TO RD-CODE
           MOVE TBLT-CASH-OUT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TBLT-KEY TO RD-KEY
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
           DISPLAY 'TLLNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TLLNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
