      *================================================================*
      * PROGRAM  : TLLMTHE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELLER BALANCING - MONTH-END. READS THE TBL         *
      *            TRANSACTIONS, UPDATES THE TBL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TELLER CASH DRAWER BALANCING,   *
      *            BATCH PROOF AND OVER/SHORT                          *
      * INPUT    : TBLMST (KSDS), TBLTRN, BANKCTL                      *
      * OUTPUT   : TBLOUT, TLLMTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TLDTE, TLPEN            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-05-06 MKL  ORIGINAL                                       *
      * 1991-02-18 RJT  PERFORMANCE - OVERSHORT READ AHEAD PRB4230     *
      * 1997-08-27 GAF  PERFORMANCE - BATCH READ AHEAD INC5440         *
      * 1999-06-23 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-04 MKL  VAULT-SELL ROUNDING - HALF UP PRB8613          *
      * 2004-01-04 BHS  NEW VAULT-BUY REPORT COLUMNS PRB8435           *
      * 2004-03-10 DPS  VAULT-SELL LIMIT REVISED INC4532               *
      * 2004-03-26 RJT  OVERSHORT TABLE EXPANDED REQ7123               *
      * 2005-10-11 SKN  CASH-IN TABLE EXPANDED INC5766                 *
      * 2007-05-07 JLM  CASH-IN LIMIT REVISED PRB1867                  *
      * 2009-01-08 GAF  CASH-IN LIMIT REVISED CHG3683                  *
      * 2010-12-06 MKL  VAULT-BUY FEE SCHEDULE UPDATE REQ1297          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLLMTHE.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "TLLMTRPT"
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
      * WS-CASH-OUT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000133500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000007000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000542000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000138000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000830500'.
       01  WS-CASH-OUT-BAND-TBL REDEFINES WS-CASH-OUT-BAND-VALUES.
           05  WS-CASH-OUT-BAND-ROW OCCURS 5 TIMES.
               10  WS-CASH-OUT-BAND-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VAULT-SE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-SE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000612500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000783000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000718000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000864000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000776500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013800000000861000'.
       01  WS-VAULT-SE-TIER-TBL REDEFINES WS-VAULT-SE-TIER-VALUES.
           05  WS-VAULT-SE-TIER-ROW OCCURS 6 TIMES.
               10  WS-VAULT-SE-TIER-LOW    PIC 9(09)V99.
               10  WS-VAULT-SE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-OUT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000378500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000281000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000847500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000877000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000771500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021050000000304000'.
       01  WS-CASH-OUT-GRID-TBL REDEFINES WS-CASH-OUT-GRID-VALUES.
           05  WS-CASH-OUT-GRID-ROW OCCURS 6 TIMES.
               10  WS-CASH-OUT-GRID-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BATCH-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BATCH-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000768500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000160000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000767000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000583500'.
       01  WS-BATCH-SCHED-TBL REDEFINES WS-BATCH-SCHED-VALUES.
           05  WS-BATCH-SCHED-ROW OCCURS 5 TIMES.
               10  WS-BATCH-SCHED-LOW      PIC 9(09)V99.
               10  WS-BATCH-SCHED-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TLLMTHE  TELLER BALANCING'.
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
       COPY TLDTEP.
       COPY TLPENP.

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
               PERFORM 3000-AGE-LAST-ACT
               PERFORM 3010-ACCRUE-OVERSHORT
               PERFORM 3020-EDIT-DESC
               PERFORM 3030-LIMIT-OVERSHORT
               PERFORM 3040-FLAG-BATCH
               PERFORM 3050-ACCRUE-OVERSHORT
               PERFORM 3060-COUNT-ITEM
               PERFORM 3070-SET-SOURCE
               PERFORM 3080-SET-TRAN
               PERFORM 3090-PRORATE-CASH-OUT
               PERFORM 3100-LIMIT-BATCH
               PERFORM 3110-SELECT-FEE-PCT
               PERFORM 3120-SELECT-RATE
               PERFORM 3130-AGE-LAST-STMT
               PERFORM 3140-EDIT-DESC
               PERFORM 3150-AGE-LAST-MAINT
               PERFORM 3160-SET-CHANNEL
               PERFORM 3170-SPLIT-VAULT-SELL
               PERFORM 3180-CHECK-NOTICE
               PERFORM 3190-CHECK-LAST-MAINT
               PERFORM 3200-ASSESS-EXEMPT
               PERFORM 3210-FLAG-VAULT-SELL
               PERFORM 3220-COUNT-EXCEPT
               PERFORM 3230-LIMIT-BATCH
               PERFORM 3240-SPLIT-BATCH
               PERFORM 3250-EDIT-DESC
               PERFORM 3260-LIMIT-CASH-OUT
               PERFORM 3270-AGE-NOTICE
               PERFORM 3280-SET-CHANNEL
               PERFORM 3290-COUNT-NOTICE
               PERFORM 3300-SET-CYCLE
               PERFORM 3310-CHECK-LAST-MAINT
               PERFORM 3320-SELECT-FEE-PCT
               PERFORM 3330-SPLIT-CASH-OUT
               PERFORM 3340-EDIT-CUST-ID
               PERFORM 3350-SET-SOURCE
               PERFORM 3360-SPLIT-BATCH
               PERFORM 3370-EDIT-MEMO
               PERFORM 3380-CHECK-EXPIRY
               PERFORM 3390-ACCUM-PROOF-TYPE
               PERFORM 3400-EDIT-MEMO
               PERFORM 3410-LIMIT-CASH-OUT
               PERFORM 3420-DESCRIBE-CYCLE
               PERFORM 3430-LIMIT-CASH-OUT
               PERFORM 3440-LIMIT-CASH-IN
               PERFORM 3450-COUNT-ITEM
               PERFORM 3460-DESCRIBE-CYCLE
               PERFORM 3470-AGE-LAST-MAINT
               PERFORM 3480-CHECK-LAST-PAY
               PERFORM 3490-ASSESS-EXEMPT
               PERFORM 3500-SET-CHANNEL
               PERFORM 3510-ACCRUE-CASH-IN
               PERFORM 3520-DESCRIBE-CHANNEL
               PERFORM 3530-ACCRUE-BATCH
               PERFORM 3540-ACCRUE-OVERSHORT
               PERFORM 3550-ACCRUE-BATCH
               PERFORM 3560-FLAG-CASH-IN
               PERFORM 3570-EDIT-OPERATOR
               PERFORM 3580-CHECK-EFFECTIVE
               PERFORM 3590-FLAG-OVERSHORT
               PERFORM 3600-CHECK-LAST-PAY
               PERFORM 3610-ASSESS-EXEMPT
               PERFORM 3620-EDIT-OPERATOR
               PERFORM 3630-SET-CYCLE
               PERFORM 3640-ACCRUE-BATCH
               PERFORM 3650-DESCRIBE-REASON
               PERFORM 3660-FLAG-OVERSHORT
               PERFORM 3670-PRORATE-CASH-IN
               PERFORM 3680-AGE-EXPIRY
               PERFORM 3690-FLAG-ITEM
               PERFORM 3700-PRORATE-CASH-OUT
               PERFORM 3710-EDIT-OPERATOR
               PERFORM 3720-DESCRIBE-CYCLE
               PERFORM 3730-ASSESS-REVIEW
               PERFORM 3740-AGE-EXPIRY
               PERFORM 3750-DESCRIBE-DRAWER
               PERFORM 3760-SPLIT-BATCH
               PERFORM 3770-ASSESS-REVIEW
               PERFORM 3780-LIMIT-VAULT-BUY
               PERFORM 3790-CHECK-EXPIRY
               PERFORM 3800-CHECK-EXPIRY
               PERFORM 3810-ACCRUE-CASH-IN
               PERFORM 3820-AGE-POST
               PERFORM 3830-LIMIT-CASH-IN
               PERFORM 3840-ACCRUE-BATCH
               PERFORM 3850-FLAG-VAULT-BUY
               PERFORM 3860-DESCRIBE-TRAN
               PERFORM 3870-FLAG-BATCH
               PERFORM 3880-SET-REASON
               PERFORM 3890-CHECK-EXPIRY
               PERFORM 3900-PRORATE-CASH-OUT
               PERFORM 3910-AGE-POST
               PERFORM 3920-LIMIT-BATCH
               PERFORM 3930-SELECT-RATE
               PERFORM 3940-EDIT-CUST-ID
               PERFORM 3950-LIMIT-OVERSHORT
               PERFORM 3960-SPLIT-BATCH
               PERFORM 3970-COUNT-ITEM
               PERFORM 3980-COUNT-NOTICE
               PERFORM 3990-ACCRUE-OVERSHORT
               PERFORM 4000-ACCUM-REASON
               PERFORM 4010-FLAG-ITEM
               PERFORM 4020-AGE-EFF
               PERFORM 4030-SELECT-RATE
               PERFORM 4040-CHECK-LAST-MAINT
               PERFORM 4050-COUNT-SEQ
               PERFORM 4060-DESCRIBE-REASON
               PERFORM 4070-EDIT-CUST-ID
               PERFORM 4080-ASSESS-HOLD
               PERFORM 4090-COUNT-EXCEPT
               PERFORM 4100-FLAG-ITEM
               PERFORM 4110-EDIT-DESC
               PERFORM 4120-AGE-EFFECTIVE
               PERFORM 4130-SET-SOURCE
               PERFORM 4140-DESCRIBE-REASON
               PERFORM 4150-ACCRUE-VAULT-SELL
               PERFORM 4160-LIMIT-VAULT-SELL
               PERFORM 4170-AGE-OPEN
               PERFORM 4180-ASSESS-STOP
               PERFORM 4190-DESCRIBE-SOURCE
               PERFORM 7010-CALL-TLDTE
               PERFORM 7040-CALL-TLPEN
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
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-LAST-ACT.
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
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E172 DESC REQUIRED
      * E064 VAULT SELL AMT NEGATIVE
      * E185 VAULT SELL AMT OVER 250000
      * E207 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-AMT < 0
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-AMT > 250000.00
                   MOVE 'E185' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3030-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-YTD < 0.00
               MOVE 0.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-YTD > 50000.00
               MOVE 50000.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3040-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLM-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 12
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN NOTICE DATE IS PAST
      * PT -> SZ WHEN NOTICE DATE IS PAST
      * SZ -> VK WHEN NOTICE DATE IS PAST
      * VK -> VW WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN POST DATE IS PAST
      * BO -> VB WHEN POST DATE IS PAST
      * VB -> VS WHEN POST DATE IS PAST
      * VS -> OS WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BATCH YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-BATCH.
           IF TBLM-BATCH-YTD < 100.00
               MOVE 100.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-YTD > 10000.00
               MOVE 10000.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT BAND TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-BAND-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-OVERSHORT-AMT NOT < WS-CASH-OUT-BAND-LOW(WS-TX)
                   MOVE WS-CASH-OUT-BAND-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE VAULT BUY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TBLM-VAULT-BUY-YTD NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-LAST-STMT.
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
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E544 DESC REQUIRED
      * E475 OVERSHORT AMT NEGATIVE
      * E877 OVERSHORT AMT OVER 25000
      * E840 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN TBLT-OVERSHORT-AMT < 0
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN TBLT-OVERSHORT-AMT > 25000.00
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN TBLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN NOTICE DATE IS PAST
      * F -> M WHEN NOTICE DATE IS PAST
      * M -> N WHEN NOTICE DATE IS PAST
      * N -> R WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-NOTICE.
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
               MOVE 'Y' TO TBLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3200-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-OUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLM-CASH-OUT-YTD ROUNDED = WS-WORK-AMT-2 *
                       TBLM-RATE / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 12.00 TO TBLM-CASH-OUT-YTD
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 10.00 TO TBLM-CASH-OUT-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-OUT-YTD ROUNDED = WS-WORK-AMT-2 *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-CASH-OUT-YTD > 0 AND TBLM-CASH-OUT-YTD < 5.00
               MOVE 5.00 TO TBLM-CASH-OUT-YTD
           END-IF
           IF TBLM-CASH-OUT-YTD > 100.00
               MOVE 100.00 TO TBLM-CASH-OUT-YTD
           END-IF
           ADD TBLM-CASH-OUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * VAULT SELL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3210-FLAG-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-SELL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-BATCH.
           IF TBLM-BATCH-YTD < 100.00
               MOVE 100.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-YTD > 5000.00
               MOVE 5000.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E109 DESC REQUIRED
      * E598 VAULT BUY YTD NEGATIVE
      * E009 VAULT BUY YTD OVER 99999
      * E946 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-YTD < 0
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-YTD > 99999.00
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN TBLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E946' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-CASH-OUT.
           IF TBLT-CASH-OUT-AMT < 100.00
               MOVE 100.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-CASH-OUT-AMT > 5000.00
               MOVE 5000.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-NOTICE.
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
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN OPEN DATE IS PAST
      * TA -> TG WHEN OPEN DATE IS PAST
      * TG -> WG WHEN OPEN DATE IS PAST
      * WG -> KX WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BATCH SCHED TIER FOR THE OVERSHORT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BATCH-SCHED-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-OVERSHORT-YTD NOT < WS-BATCH-SCHED-LOW(WS-TX)
                   MOVE WS-BATCH-SCHED-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3330-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TBLT-CASH-OUT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLT-CASH-OUT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E360 CUST ID REQUIRED
      * E289 OVERSHORT AMT NEGATIVE
      * E056 OVERSHORT AMT OVER 25000
      * E875 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CUST-ID = SPACES
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT < 0
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT > 25000.00
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN EFF DATE IS PAST
      * PT -> SZ WHEN EFF DATE IS PAST
      * SZ -> VK WHEN EFF DATE IS PAST
      * VK -> VW WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-BATCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E646 MEMO REQUIRED
      * E477 CASH OUT YTD NEGATIVE
      * E668 CASH OUT YTD OVER 1000000
      * E126 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3370-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-MEMO = SPACES
                   MOVE 'E646' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD < 0
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD > 1000000.00
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL AMT BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD TBLM-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E006 MEMO REQUIRED
      * E565 WORK AMT 2 NEGATIVE
      * E600 WORK AMT 2 OVER 5000
      * E567 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-MEMO = SPACES
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3410-LIMIT-CASH-OUT.
           IF TBLT-CASH-OUT-AMT < 0.00
               MOVE 0.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-CASH-OUT-AMT > 500.00
               MOVE 500.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3430-LIMIT-CASH-OUT.
           IF TBLT-CASH-OUT-AMT < 50.00
               MOVE 50.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-CASH-OUT-AMT > 2500.00
               MOVE 2500.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-YTD < 100.00
               MOVE 100.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-YTD > 50000.00
               MOVE 50000.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE VAULT BUY YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3490-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-VAULT-BUY-YTD
                       * TBLM-FEE-PCT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 10.00 TO TBLM-CASH-IN-YTD
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 3.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-VAULT-BUY-YTD
                       * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 10.00
               MOVE 10.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 100.00
               MOVE 100.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-CASH-IN.
           IF TBLM-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-BATCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-BATCH.
           IF TBLT-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-BATCH-AMT * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLT-VAULT-SELL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-YTD * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-IN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-BATCH.
           IF TBLT-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-BATCH-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-VAULT-SELL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3560-FLAG-CASH-IN.
           IF TBLM-CASH-IN-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-IN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E651 OPERATOR REQUIRED
      * E681 CASH IN AMT NEGATIVE
      * E981 CASH IN AMT OVER 250000
      * E828 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E651' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 250000.00
                   MOVE 'E981' TO WS-REJECT-CODE
               WHEN TBLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3590-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3610-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLT-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-FEE-PCT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 15.00 TO TBLT-OVERSHORT-AMT
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 20.00 TO TBLT-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLT-OVERSHORT-AMT > 0 AND TBLT-OVERSHORT-AMT < 2.00
               MOVE 2.00 TO TBLT-OVERSHORT-AMT
           END-IF
           IF TBLT-OVERSHORT-AMT > 100.00
               MOVE 100.00 TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E176 OPERATOR REQUIRED
      * E073 WORK AMT 2 NEGATIVE
      * E311 WORK AMT 2 OVER 5000
      * E335 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN TBLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN LAST PAY DATE IS PAST
      * TA -> TG WHEN LAST PAY DATE IS PAST
      * TG -> WG WHEN LAST PAY DATE IS PAST
      * WG -> KX WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-BATCH.
           IF TBLM-BATCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-YTD * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-VAULT-BUY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3660-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                   TBLM-OVERSHORT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EXPIRY.
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
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3690-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-OUT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E606 OPERATOR REQUIRED
      * E533 CASH OUT AMT NEGATIVE
      * E113 CASH OUT AMT OVER 99999
      * E508 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT < 0
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT > 99999.00
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E508' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE BATCH AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3730-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TBLT-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED = TBLM-BATCH-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO TBLT-CASH-OUT-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO TBLT-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED = TBLM-BATCH-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLT-CASH-OUT-AMT > 0 AND TBLT-CASH-OUT-AMT < 2.00
               MOVE 2.00 TO TBLT-CASH-OUT-AMT
           END-IF
           IF TBLT-CASH-OUT-AMT > 100.00
               MOVE 100.00 TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-EXPIRY.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLT-BATCH-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLT-BATCH-AMT - (WS-WORK-AMT * 3)
           COMPUTE TBLM-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3770-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-DISCOUNT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 15.00 TO TBLM-CASH-OUT-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 20.00 TO TBLM-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-OUT-AMT > 0 AND TBLM-CASH-OUT-AMT < 2.00
               MOVE 2.00 TO TBLM-CASH-OUT-AMT
           END-IF
           IF TBLM-CASH-OUT-AMT > 50.00
               MOVE 50.00 TO TBLM-CASH-OUT-AMT
           END-IF
           ADD TBLM-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * VAULT BUY AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3780-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT < 50.00
               MOVE 50.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 500.00
               MOVE 500.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-CASH-IN.
           IF TBLM-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLT-VAULT-SELL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-POST.
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
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3830-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-AMT < 50.00
               MOVE 50.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-AMT > 2500.00
               MOVE 2500.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-BATCH.
           IF TBLM-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-CASH-OUT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3850-FLAG-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-BUY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BO'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'OS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-BATCH.
           IF TBLM-BATCH-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN POST DATE IS PAST
      * GV -> PG WHEN POST DATE IS PAST
      * PG -> SX WHEN POST DATE IS PAST
      * SX -> XN WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SX' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SX'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XN' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-EXPIRY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3900-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TBLM-CASH-OUT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-POST.
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
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3920-LIMIT-BATCH.
           IF TBLM-BATCH-AMT < 10.00
               MOVE 10.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-AMT > 2500.00
               MOVE 2500.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE TIER TIER FOR THE VAULT SELL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-TIER-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TBLM-VAULT-SELL-YTD NOT < WS-VAULT-SE-TIER-LOW(WS-TX)
                   MOVE WS-VAULT-SE-TIER-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E005 CUST ID REQUIRED
      * E353 OVERSHORT YTD NEGATIVE
      * E914 OVERSHORT YTD OVER 10000
      * E983 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CUST-ID = SPACES
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD < 0
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD > 10000.00
                   MOVE 'E914' TO WS-REJECT-CODE
               WHEN TBLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-AMT < 100.00
               MOVE 100.00 TO TBLM-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-AMT > 50000.00
               MOVE 50000.00 TO TBLM-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE TBLM-BATCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 5
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLT-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4010-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-EFF.
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
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT BAND TIER FOR THE VAULT SELL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-BAND-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-VAULT-SELL-YTD NOT < WS-CASH-OUT-BAND-LOW(WS-TX)
                   MOVE WS-CASH-OUT-BAND-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TBLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 5
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E989 CUST ID REQUIRED
      * E668 WORK AMT 2 NEGATIVE
      * E984 WORK AMT 2 OVER 1000000
      * E965 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CUST-ID = SPACES
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE VAULT SELL YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4080-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLT-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-VAULT-SELL-YTD
                       * TBLM-DISCOUNT / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 20.00 TO TBLT-BATCH-AMT
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 15.00 TO TBLT-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-VAULT-SELL-YTD
                       * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLT-BATCH-AMT > 0 AND TBLT-BATCH-AMT < 2.00
               MOVE 2.00 TO TBLT-BATCH-AMT
           END-IF
           IF TBLT-BATCH-AMT > 100.00
               MOVE 100.00 TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4100-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E436 DESC REQUIRED
      * E798 VAULT SELL YTD NEGATIVE
      * E181 VAULT SELL YTD OVER 1000000
      * E513 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD < 0
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD > 1000000.00
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN TBLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-EFFECTIVE.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN OPEN DATE IS PAST
      * PT -> SZ WHEN OPEN DATE IS PAST
      * SZ -> VK WHEN OPEN DATE IS PAST
      * VK -> VW WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-SOURCE.
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
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-VAULT-SELL-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-CASH-OUT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4160-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT < 10.00
               MOVE 10.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 500.00
               MOVE 500.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4180-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLT-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO TBLT-OVERSHORT-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO TBLT-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLT-OVERSHORT-AMT > 0 AND TBLT-OVERSHORT-AMT < 2.00
               MOVE 2.00 TO TBLT-OVERSHORT-AMT
           END-IF
           IF TBLT-OVERSHORT-AMT > 250.00
               MOVE 250.00 TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TLDTE.
           INITIALIZE TLDTE-PARMS
           MOVE TBLM-CASH-IN-YTD
             TO TLDTE-CASH-IN-YTD
           MOVE TBLM-CASH-OUT-YTD
             TO TLDTE-CASH-OUT-YTD
           MOVE TBLM-CASH-OUT-AMT
             TO TLDTE-CASH-OUT-AMT
           MOVE TBLM-VAULT-BUY-YTD
             TO TLDTE-VAULT-BUY-YTD
           MOVE TBLM-BATCH-AMT
             TO TLDTE-BATCH-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLDTE-CASH-IN-AMT
           MOVE TBLM-LAST-STMT-DATE
             TO TLDTE-LAST-STMT-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLDTE-LAST-ACT-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLDTE-NOTICE-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLDTE-EFFECTIVE-DATE
           MOVE TBLM-CYCLE-CD
             TO TLDTE-CYCLE-CD
           MOVE TBLM-CHANNEL-CD
             TO TLDTE-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLDTE-SOURCE-CD
           MOVE TBLM-DRAWER-CD
             TO TLDTE-DRAWER-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLDTE-SUSPEND-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLDTE-HOLD-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLDTE-STOP-FLAG
           MOVE TBLM-NOTICE-CNT
             TO TLDTE-NOTICE-CNT
           MOVE TBLM-RETRY-CNT
             TO TLDTE-RETRY-CNT
           MOVE TBLM-DISCOUNT
             TO TLDTE-DISCOUNT
           MOVE TBLM-FEE-PCT
             TO TLDTE-FEE-PCT
           MOVE TBLM-DESC
             TO TLDTE-DESC
           MOVE TBLM-CONTACT
             TO TLDTE-CONTACT
           CALL 'TLDTE' USING TLDTE-PARMS
           EVALUATE TLDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLDTE-CASH-IN-YTD
                     TO TBLM-CASH-IN-YTD
                   MOVE TLDTE-CASH-OUT-AMT
                     TO TBLM-CASH-OUT-AMT
                   MOVE TLDTE-CASH-IN-AMT
                     TO TBLM-CASH-IN-AMT
                   MOVE TLDTE-SUSPEND-FLAG
                     TO TBLM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TLDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-TLPEN.
           INITIALIZE TLPEN-PARMS
           MOVE TBLM-OVERSHORT-AMT
             TO TLPEN-OVERSHORT-AMT
           MOVE TBLM-VAULT-BUY-AMT
             TO TLPEN-VAULT-BUY-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLPEN-CASH-IN-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLPEN-CASH-OUT-YTD
           MOVE TBLM-BATCH-AMT
             TO TLPEN-BATCH-AMT
           MOVE TBLM-CASH-OUT-AMT
             TO TLPEN-CASH-OUT-AMT
           MOVE TBLM-OPEN-DATE
             TO TLPEN-OPEN-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLPEN-EFFECTIVE-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLPEN-NOTICE-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLPEN-NEXT-REVIEW-DATE
           MOVE TBLM-DRAWER-CD
             TO TLPEN-DRAWER-CD
           MOVE TBLM-REASON-CD
             TO TLPEN-REASON-CD
           MOVE TBLM-CHANNEL-CD
             TO TLPEN-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLPEN-SOURCE-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLPEN-SUSPEND-FLAG
           MOVE TBLM-REVIEW-FLAG
             TO TLPEN-REVIEW-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLPEN-HOLD-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLPEN-ITEM-CNT
           MOVE TBLM-RETRY-CNT
             TO TLPEN-RETRY-CNT
           MOVE TBLM-RATE
             TO TLPEN-RATE
           MOVE TBLM-FEE-PCT
             TO TLPEN-FEE-PCT
           MOVE TBLM-REF-NO
             TO TLPEN-REF-NO
           MOVE TBLM-DESC
             TO TLPEN-DESC
           CALL 'TLPEN' USING TLPEN-PARMS
           EVALUATE TLPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLPEN-OVERSHORT-AMT
                     TO TBLM-OVERSHORT-AMT
                   MOVE TLPEN-VAULT-BUY-AMT
                     TO TBLM-VAULT-BUY-AMT
                   MOVE TLPEN-CASH-IN-AMT
                     TO TBLM-CASH-IN-AMT
                   MOVE TLPEN-BATCH-AMT
                     TO TBLM-BATCH-AMT
                   MOVE TLPEN-REVIEW-FLAG
                     TO TBLM-REVIEW-FLAG
                   MOVE TLPEN-HOLD-FLAG
                     TO TBLM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-TLPEN' TO ABND-PARAGRAPH
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
           DISPLAY 'TLLMTHE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TLLMTHE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
