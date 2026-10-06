      *================================================================*
      * PROGRAM  : TLLSWEP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELLER BALANCING - SWEEP. READS THE TBL             *
      *            TRANSACTIONS, UPDATES THE TBL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TELLER CASH DRAWER BALANCING,   *
      *            BATCH PROOF AND OVER/SHORT                          *
      * INPUT    : TBLMST (KSDS), TBLTRN, BANKCTL                      *
      * OUTPUT   : TBLOUT, TLLSWRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TLELG, TLFEE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-11-06 MKL  ORIGINAL                                       *
      * 1996-05-26 TWH  OVERSHORT STATUS CODES ADDED REQ4597           *
      * 1997-11-17 RJT  BATCH CALCULATION CORRECTED REQ2016            *
      * 2001-11-25 BHS  PERFORMANCE - BATCH READ AHEAD INC3425         *
      * 2001-08-04 CRP  CASH-OUT LIMIT REVISED CHG1361                 *
      * 2002-11-14 MKL  PERFORMANCE - CASH-IN READ AHEAD REQ8145       *
      * 2003-06-03 LMB  VAULT-BUY NOTICE TEXT PER LEGAL INC3675        *
      * 2004-12-27 MKL  OVERSHORT LIMIT REVISED PRB1184                *
      * 2004-08-10 TWH  PERFORMANCE - VAULT-SELL READ AHEAD INC9865    *
      * 2007-11-13 EWS  VAULT-SELL EXCEPTIONS TO OPS QUEUE PRB1217     *
      * 2008-02-11 RJT  PERFORMANCE - BATCH READ AHEAD PRB2387         *
      * 2011-09-18 SKN  AUDIT FINDING - VAULT-BUY CONTROL TOTALS INC135*
      * 2011-02-26 EWS  CASH-OUT CALCULATION CORRECTED INC0808         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLLSWEP.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "TLLSWRPT"
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
                                           VALUE 14.000000.
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
      * WS-VAULT-BU-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-BU-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000882500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000582500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000620000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000226500'.
       01  WS-VAULT-BU-BAND-TBL REDEFINES WS-VAULT-BU-BAND-VALUES.
           05  WS-VAULT-BU-BAND-ROW OCCURS 4 TIMES.
               10  WS-VAULT-BU-BAND-LOW    PIC 9(09)V99.
               10  WS-VAULT-BU-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-OUT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000761500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000851500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000204000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000344000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001650000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011650000000713000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021650000000275500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024150000000866500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024250000000184000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024500000000188000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029500000000258500'.
       01  WS-CASH-OUT-TIER-TBL REDEFINES WS-CASH-OUT-TIER-VALUES.
           05  WS-CASH-OUT-TIER-ROW OCCURS 11 TIMES.
               10  WS-CASH-OUT-TIER-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-IN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-IN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000082000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000387500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000085000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000692500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000721500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000229500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000184000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000260000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016450000000044000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000182500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021500000000070000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000252000'.
       01  WS-CASH-IN-TIER-TBL REDEFINES WS-CASH-IN-TIER-VALUES.
           05  WS-CASH-IN-TIER-ROW OCCURS 12 TIMES.
               10  WS-CASH-IN-TIER-LOW     PIC 9(09)V99.
               10  WS-CASH-IN-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VAULT-SE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-SE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000828000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000443500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000217500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000717000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025750000000100500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028250000000564000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038250000000545000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00048250000000383000'.
       01  WS-VAULT-SE-SCHED-TBL REDEFINES WS-VAULT-SE-SCHED-VALUES.
           05  WS-VAULT-SE-SCHED-ROW OCCURS 9 TIMES.
               10  WS-VAULT-SE-SCHED-LOW   PIC 9(09)V99.
               10  WS-VAULT-SE-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TLLSWEP  TELLER BALANCING'.
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
       COPY TLELGP.
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
               PERFORM 3000-CHECK-LAST-ACT
               PERFORM 3010-ASSESS-STOP
               PERFORM 3020-SPLIT-CASH-IN
               PERFORM 3030-CHECK-LAST-ACT
               PERFORM 3040-SELECT-DISCOUNT
               PERFORM 3050-DESCRIBE-REASON
               PERFORM 3060-EDIT-REJECT
               PERFORM 3070-ACCUM-CYCLE
               PERFORM 3080-FLAG-CASH-OUT
               PERFORM 3090-SET-CHANNEL
               PERFORM 3100-SET-PROOF-TYPE
               PERFORM 3110-LIMIT-VAULT-BUY
               PERFORM 3120-ACCUM-SOURCE
               PERFORM 3130-FLAG-OVERSHORT
               PERFORM 3140-ACCRUE-CASH-IN
               PERFORM 3150-FLAG-VAULT-BUY
               PERFORM 3160-AGE-NOTICE
               PERFORM 3170-FLAG-OVERSHORT
               PERFORM 3180-ACCRUE-VAULT-SELL
               PERFORM 3190-DESCRIBE-REASON
               PERFORM 3200-SET-CHANNEL
               PERFORM 3210-LIMIT-CASH-OUT
               PERFORM 3220-ACCUM-CHANNEL
               PERFORM 3230-EDIT-REJECT
               PERFORM 3240-SET-CHANNEL
               PERFORM 3250-LIMIT-OVERSHORT
               PERFORM 3260-DESCRIBE-CHANNEL
               PERFORM 3270-FLAG-ITEM
               PERFORM 3280-LIMIT-CASH-IN
               PERFORM 3290-AGE-POST
               PERFORM 3300-AGE-LAST-PAY
               PERFORM 3310-LIMIT-VAULT-SELL
               PERFORM 3320-ASSESS-REVERSAL
               PERFORM 3330-ASSESS-HOLD
               PERFORM 3340-AGE-LAST-ACT
               PERFORM 3350-EDIT-DESC
               PERFORM 3360-FLAG-CASH-OUT
               PERFORM 3370-ACCUM-PROOF-TYPE
               PERFORM 3380-DESCRIBE-CHANNEL
               PERFORM 3390-PRORATE-VAULT-SELL
               PERFORM 3400-ACCUM-CHANNEL
               PERFORM 3410-ASSESS-OVERRIDE
               PERFORM 3420-ACCRUE-BATCH
               PERFORM 3430-CHECK-OPEN
               PERFORM 3440-AGE-LAST-MAINT
               PERFORM 3450-ACCRUE-VAULT-SELL
               PERFORM 3460-CHECK-LAST-ACT
               PERFORM 3470-FLAG-CASH-OUT
               PERFORM 3480-AGE-POST
               PERFORM 3490-CHECK-POST
               PERFORM 3500-SPLIT-CASH-IN
               PERFORM 3510-EDIT-DESC
               PERFORM 3520-CHECK-LAST-MAINT
               PERFORM 3530-SET-REASON
               PERFORM 3540-ACCRUE-VAULT-SELL
               PERFORM 3550-AGE-EXPIRY
               PERFORM 3560-ACCRUE-CASH-OUT
               PERFORM 3570-DESCRIBE-CHANNEL
               PERFORM 3580-SPLIT-BATCH
               PERFORM 3590-SPLIT-VAULT-BUY
               PERFORM 3600-COUNT-RETRY
               PERFORM 3610-EDIT-CONTACT
               PERFORM 3620-ACCRUE-OVERSHORT
               PERFORM 3630-DESCRIBE-CYCLE
               PERFORM 3640-LIMIT-OVERSHORT
               PERFORM 3650-COUNT-NOTICE
               PERFORM 3660-ACCRUE-CASH-OUT
               PERFORM 3670-COUNT-ITEM
               PERFORM 3680-DESCRIBE-CHANNEL
               PERFORM 3690-ACCUM-PROOF-TYPE
               PERFORM 3700-AGE-OPEN
               PERFORM 3710-SET-CHANNEL
               PERFORM 3720-LIMIT-VAULT-SELL
               PERFORM 3730-SELECT-FEE-PCT
               PERFORM 3740-COUNT-RETRY
               PERFORM 3750-ACCUM-REASON
               PERFORM 3760-LIMIT-CASH-OUT
               PERFORM 3770-PRORATE-OVERSHORT
               PERFORM 3780-SELECT-DISCOUNT
               PERFORM 3790-AGE-EFFECTIVE
               PERFORM 3800-FLAG-OVERSHORT
               PERFORM 3810-LIMIT-OVERSHORT
               PERFORM 3820-SPLIT-CASH-OUT
               PERFORM 3830-PRORATE-CASH-OUT
               PERFORM 3840-CHECK-NEXT-REVIEW
               PERFORM 3850-FLAG-ITEM
               PERFORM 3860-LIMIT-CASH-OUT
               PERFORM 3870-ACCUM-SOURCE
               PERFORM 3880-ASSESS-EXEMPT
               PERFORM 3890-EDIT-CONTACT
               PERFORM 3900-PRORATE-BATCH
               PERFORM 3910-ACCUM-REASON
               PERFORM 3920-AGE-OPEN
               PERFORM 3930-SELECT-RATE
               PERFORM 3940-SPLIT-BATCH
               PERFORM 3950-FLAG-BATCH
               PERFORM 3960-ACCRUE-VAULT-BUY
               PERFORM 3970-SPLIT-VAULT-SELL
               PERFORM 7030-CALL-TLELG
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
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH YTD FEE ON THE CASH OUT YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3010-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLM-CASH-OUT-YTD *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 7.00 TO TBLM-BATCH-YTD
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 5.00 TO TBLM-BATCH-YTD
               WHEN OTHER
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLM-CASH-OUT-YTD *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-BATCH-YTD > 0 AND TBLM-BATCH-YTD < 5.00
               MOVE 5.00 TO TBLM-BATCH-YTD
           END-IF
           IF TBLM-BATCH-YTD > 35.00
               MOVE 35.00 TO TBLM-BATCH-YTD
           END-IF
           ADD TBLM-BATCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASH IN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TBLM-CASH-IN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-IN-AMT - (WS-WORK-AMT * 4)
           COMPUTE TBLM-VAULT-BUY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU BAND TIER FOR THE CASH OUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-BAND-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLT-CASH-OUT-AMT NOT < WS-VAULT-BU-BAND-LOW(WS-TX)
                   MOVE WS-VAULT-BU-BAND-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E966 REJECT CD REQUIRED
      * E254 BATCH AMT NEGATIVE
      * E825 BATCH AMT OVER 1000000
      * E578 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E966' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT < 0
                   MOVE 'E254' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT > 1000000.00
                   MOVE 'E825' TO WS-REJECT-CODE
               WHEN TBLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CYCLE-CD
                   ADD TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3080-FLAG-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLT-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN OPEN DATE IS PAST
      * F -> M WHEN OPEN DATE IS PAST
      * M -> N WHEN OPEN DATE IS PAST
      * N -> R WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN EFF DATE IS PAST
      * RX -> TW WHEN EFF DATE IS PAST
      * TW -> UG WHEN EFF DATE IS PAST
      * UG -> XY WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RX' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TW' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XY' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3110-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT < 0.00
               MOVE 0.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 5000.00
               MOVE 5000.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-SOURCE-CD
                   ADD TBLT-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3130-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-CASH-IN.
           IF TBLM-CASH-IN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-IN-AMT * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLT-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3150-FLAG-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-BUY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-NOTICE.
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
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3170-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN OPEN DATE IS PAST
      * F -> M WHEN OPEN DATE IS PAST
      * M -> N WHEN OPEN DATE IS PAST
      * N -> R WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3210-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 0.00
               MOVE 0.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 1000.00
               MOVE 1000.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E652 REJECT CD REQUIRED
      * E719 BATCH AMT NEGATIVE
      * E757 BATCH AMT OVER 5000
      * E935 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT < 0
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT > 5000.00
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN TBLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-YTD < 10.00
               MOVE 10.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-YTD > 50000.00
               MOVE 50000.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3270-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3280-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-AMT < 25.00
               MOVE 25.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-AMT > 10000.00
               MOVE 10000.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-POST.
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
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3310-LIMIT-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT < 10.00
               MOVE 10.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-VAULT-SELL-AMT > 500.00
               MOVE 500.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT YTD FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3320-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-OUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE TBLM-CASH-OUT-YTD ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-FEE-PCT / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 15.00 TO TBLM-CASH-OUT-YTD
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 3.00 TO TBLM-CASH-OUT-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-OUT-YTD ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-CASH-OUT-YTD > 0 AND TBLM-CASH-OUT-YTD < 5.00
               MOVE 5.00 TO TBLM-CASH-OUT-YTD
           END-IF
           IF TBLM-CASH-OUT-YTD > 50.00
               MOVE 50.00 TO TBLM-CASH-OUT-YTD
           END-IF
           ADD TBLM-CASH-OUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT YTD FEE ON THE VAULT SELL YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3330-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-YTD ROUNDED =
                       TBLM-VAULT-SELL-YTD * TBLM-DISCOUNT / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 15.00 TO TBLM-OVERSHORT-YTD
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 5.00 TO TBLM-OVERSHORT-YTD
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-YTD ROUNDED =
                       TBLM-VAULT-SELL-YTD * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-YTD > 0 AND TBLM-OVERSHORT-YTD < 5.00
               MOVE 5.00 TO TBLM-OVERSHORT-YTD
           END-IF
           IF TBLM-OVERSHORT-YTD > 250.00
               MOVE 250.00 TO TBLM-OVERSHORT-YTD
           END-IF
           ADD TBLM-OVERSHORT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-LAST-ACT.
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
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E433 DESC REQUIRED
      * E520 CASH IN AMT NEGATIVE
      * E906 CASH IN AMT OVER 1000000
      * E576 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 1000000.00
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN TBLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3360-FLAG-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLT-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL AMT BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-PROOF-TYPE.
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
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BATCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       3390-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                   TBLM-BATCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE VAULT BUY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3410-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-VAULT-BUY-YTD
                       * TBLM-DISCOUNT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 15.00 TO TBLM-CASH-IN-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 20.00 TO TBLM-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-VAULT-BUY-YTD
                       * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-AMT > 0 AND TBLM-CASH-IN-AMT < 2.00
               MOVE 2.00 TO TBLM-CASH-IN-AMT
           END-IF
           IF TBLM-CASH-IN-AMT > 75.00
               MOVE 75.00 TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3420-ACCRUE-BATCH.
           IF TBLM-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-CASH-IN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-OPEN.
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
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-LAST-MAINT.
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
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLT-CASH-OUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3470-FLAG-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-POST.
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
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TBLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH IN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TBLM-CASH-IN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-IN-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E095 DESC REQUIRED
      * E022 CASH OUT AMT NEGATIVE
      * E797 CASH OUT AMT OVER 5000
      * E873 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT < 0
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT > 5000.00
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN POST DATE IS PAST
      * GV -> PG WHEN POST DATE IS PAST
      * PG -> SX WHEN POST DATE IS PAST
      * SX -> XN WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-REASON.
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
      * DAILY INTEREST ON VAULT SELL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-EXPIRY.
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
      * DAILY INTEREST ON CASH OUT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-CASH-OUT.
           IF TBLM-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-OUT-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-AMT - (WS-WORK-AMT * 4)
           COMPUTE TBLM-BATCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-BUY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-BUY-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TBLM-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E219 CONTACT REQUIRED
      * E908 CASH OUT YTD NEGATIVE
      * E374 CASH OUT YTD OVER 50000
      * E774 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD < 0
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD > 50000.00
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN TBLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E774' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-YTD * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3640-LIMIT-OVERSHORT.
           IF TBLT-OVERSHORT-AMT < 10.00
               MOVE 10.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-OVERSHORT-AMT > 2500.00
               MOVE 2500.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-CASH-OUT-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT YTD BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD TBLM-CASH-OUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-OUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-OPEN.
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
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN EFF DATE IS PAST
      * F -> M WHEN EFF DATE IS PAST
      * M -> N WHEN EFF DATE IS PAST
      * N -> R WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT < 25.00
               MOVE 25.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-VAULT-SELL-AMT > 1000.00
               MOVE 1000.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT TIER TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-TIER-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TBLM-OVERSHORT-AMT NOT < WS-CASH-OUT-TIER-LOW(WS-TX)
                   MOVE WS-CASH-OUT-TIER-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLM-BATCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-BATCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 25.00
               MOVE 25.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 10000.00
               MOVE 10000.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT YTD.
      *----------------------------------------------------------------*
       3770-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-YTD ROUNDED =
                   TBLT-CASH-OUT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-YTD
           END-IF
           ADD TBLM-OVERSHORT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE SCHED TIER FOR THE BATCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-SCHED-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLM-BATCH-YTD NOT < WS-VAULT-SE-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-SE-SCHED-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3810-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-AMT < 25.00
               MOVE 25.00 TO TBLM-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-AMT > 500.00
               MOVE 500.00 TO TBLM-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TBLM-CASH-OUT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-OUT-AMT - (WS-WORK-AMT * 3)
           COMPUTE TBLM-BATCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3850-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-CASH-OUT.
           IF TBLT-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-CASH-OUT-AMT > 10000.00
               MOVE 10000.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-SOURCE-CD
                   ADD TBLT-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE BATCH AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3880-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLT-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLT-VAULT-SELL-AMT ROUNDED = TBLM-BATCH-AMT
                       * TBLM-RATE / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 3.00 TO TBLT-VAULT-SELL-AMT
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 7.00 TO TBLT-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TBLT-VAULT-SELL-AMT ROUNDED = TBLM-BATCH-AMT
                       * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-VAULT-SELL-AMT > 0 AND TBLT-VAULT-SELL-AMT < 2.00
               MOVE 2.00 TO TBLT-VAULT-SELL-AMT
           END-IF
           IF TBLT-VAULT-SELL-AMT > 50.00
               MOVE 50.00 TO TBLT-VAULT-SELL-AMT
           END-IF
           ADD TBLT-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E460 CONTACT REQUIRED
      * E048 BATCH YTD NEGATIVE
      * E128 BATCH YTD OVER 5000
      * E355 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD < 0
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD > 5000.00
                   MOVE 'E128' TO WS-REJECT-CODE
               WHEN TBLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       3900-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-BATCH-AMT ROUNDED =
                   TBLM-VAULT-BUY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLT-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-OPEN.
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
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT TIER TIER FOR THE BATCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-TIER-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TBLM-BATCH-AMT NOT < WS-CASH-OUT-TIER-LOW(WS-TX)
                   MOVE WS-CASH-OUT-TIER-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE TBLM-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3950-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT BUY AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-BUY-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLT-VAULT-SELL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE TBLT-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-TLELG.
           INITIALIZE TLELG-PARMS
           MOVE TBLM-VAULT-SELL-YTD
             TO TLELG-VAULT-SELL-YTD
           MOVE TBLM-CASH-IN-AMT
             TO TLELG-CASH-IN-AMT
           MOVE TBLM-CASH-OUT-AMT
             TO TLELG-CASH-OUT-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLELG-CASH-OUT-YTD
           MOVE TBLM-OVERSHORT-YTD
             TO TLELG-OVERSHORT-YTD
           MOVE TBLM-OVERSHORT-AMT
             TO TLELG-OVERSHORT-AMT
           MOVE TBLM-EXPIRY-DATE
             TO TLELG-EXPIRY-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLELG-LAST-ACT-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLELG-LAST-PAY-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLELG-EFFECTIVE-DATE
           MOVE TBLM-PROOF-TYPE-CD
             TO TLELG-PROOF-TYPE-CD
           MOVE TBLM-CYCLE-CD
             TO TLELG-CYCLE-CD
           MOVE TBLM-SOURCE-CD
             TO TLELG-SOURCE-CD
           MOVE TBLM-CHANNEL-CD
             TO TLELG-CHANNEL-CD
           MOVE TBLM-REVIEW-FLAG
             TO TLELG-REVIEW-FLAG
           MOVE TBLM-OVERRIDE-FLAG
             TO TLELG-OVERRIDE-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLELG-STOP-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLELG-ITEM-CNT
           MOVE TBLM-NOTICE-CNT
             TO TLELG-NOTICE-CNT
           MOVE TBLM-RATE
             TO TLELG-RATE
           MOVE TBLM-DISCOUNT
             TO TLELG-DISCOUNT
           MOVE TBLM-DESC
             TO TLELG-DESC
           MOVE TBLM-ADDR
             TO TLELG-ADDR
           CALL 'TLELG' USING TLELG-PARMS
           EVALUATE TLELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLELG-VAULT-SELL-YTD
                     TO TBLM-VAULT-SELL-YTD
                   MOVE TLELG-CASH-IN-AMT
                     TO TBLM-CASH-IN-AMT
                   MOVE TLELG-OVERSHORT-YTD
                     TO TBLM-OVERSHORT-YTD
                   MOVE TLELG-OVERSHORT-AMT
                     TO TBLM-OVERSHORT-AMT
                   MOVE TLELG-REVIEW-FLAG
                     TO TBLM-REVIEW-FLAG
                   MOVE TLELG-OVERRIDE-FLAG
                     TO TBLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-TLELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE TLFEE-VAULT-BUY-YTD
                     TO TBLM-VAULT-BUY-YTD
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
           DISPLAY 'TLLSWEP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TLLSWEP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
