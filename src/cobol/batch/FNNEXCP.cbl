      *================================================================*
      * PROGRAM  : FNNEXCP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : FOREIGN BANKNOTES - EXCEPTION PROCESSING. READS THE *
      *            FXN TRANSACTIONS, UPDATES THE FXN MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. FOREIGN CURRENCY       *
      *            BANKNOTE ORDERS, BUYBACK AND MARGIN                 *
      * INPUT    : FXNMST (KSDS), FXNTRN, BANKCTL                      *
      * OUTPUT   : FXNOUT, FNNEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, FNELG                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-17 LMB  ORIGINAL                                       *
      * 1997-10-14 JLM  BUYBACK TABLE EXPANDED REQ9231                 *
      * 1999-02-18 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-03 LMB  FOREIGN STATUS CODES ADDED INC8350             *
      * 2000-06-21 DPS  AUDIT FINDING - MARGIN CONTROL TOTALS INC6847  *
      * 2000-11-07 JLM  MARGIN ROUNDING - HALF UP PRB9420              *
      * 2003-03-01 TWH  MARGIN CUTOFF MOVED INC0107                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNNEXCP.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "FXNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FXNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "FXNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "FXNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "FNNEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY FXNMST.
       FD  TRAN-FILE.
       COPY FXNTRN.
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
                                           VALUE 24.000000.
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
      * WS-SHIPPING-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPPING-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000863500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000701000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000234500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000778000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003150000000691500'.
       01  WS-SHIPPING-SCHED-TBL REDEFINES WS-SHIPPING-SCHED-VALUES.
           05  WS-SHIPPING-SCHED-ROW OCCURS 5 TIMES.
               10  WS-SHIPPING-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHIPPING-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHIPPING-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPPING-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000146000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000840000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000641500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000536500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000114500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021600000000849000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021700000000271500'.
       01  WS-SHIPPING-TIER-TBL REDEFINES WS-SHIPPING-TIER-VALUES.
           05  WS-SHIPPING-TIER-ROW OCCURS 7 TIMES.
               10  WS-SHIPPING-TIER-LOW    PIC 9(09)V99.
               10  WS-SHIPPING-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-USD-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-USD-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000178000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000850000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000863000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000639000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000141000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000345500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004250000000747500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004750000000732500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004800000000488000'.
       01  WS-USD-TIER-TBL REDEFINES WS-USD-TIER-VALUES.
           05  WS-USD-TIER-ROW OCCURS 9 TIMES.
               10  WS-USD-TIER-LOW         PIC 9(09)V99.
               10  WS-USD-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUYBACK-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000360500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000441000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000606000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000255500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000476500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027500000000046000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027550000000485500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037550000000465500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040050000000877500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040100000000736000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040350000000629000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040850000000080000'.
       01  WS-BUYBACK-SCHED-TBL REDEFINES WS-BUYBACK-SCHED-VALUES.
           05  WS-BUYBACK-SCHED-ROW OCCURS 12 TIMES.
               10  WS-BUYBACK-SCHED-LOW    PIC 9(09)V99.
               10  WS-BUYBACK-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  FNNEXCP  FOREIGN BANKNOTES'.
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
       COPY FNELGP.

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
               MOVE 'FXNMST' TO ABND-FILE-NAME
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
           MOVE FXNT-KEY TO FXNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-BRANCH
               PERFORM 3010-DESCRIBE-CHANNEL
               PERFORM 3020-COUNT-EXCEPT
               PERFORM 3030-DESCRIBE-SOURCE
               PERFORM 3040-EDIT-NAME
               PERFORM 3050-EDIT-NAME
               PERFORM 3060-EDIT-OPERATOR
               PERFORM 3070-FLAG-MARGIN
               PERFORM 3080-ACCRUE-MARGIN
               PERFORM 3090-LIMIT-USD
               PERFORM 3100-SPLIT-BUYBACK
               PERFORM 3110-LIMIT-SHIPPING
               PERFORM 3120-EDIT-DESC
               PERFORM 3130-EDIT-MEMO
               PERFORM 3140-ACCUM-STATUS
               PERFORM 3150-SPLIT-BUYBACK
               PERFORM 3160-EDIT-OPERATOR
               PERFORM 3170-DESCRIBE-STATUS
               PERFORM 3180-FLAG-ITEM
               PERFORM 3190-DESCRIBE-CHANNEL
               PERFORM 3200-CHECK-EFF
               PERFORM 3210-ACCUM-STATUS
               PERFORM 3220-ACCUM-TYPE
               PERFORM 3230-ACCUM-CHANNEL
               PERFORM 3240-DESCRIBE-TRAN
               PERFORM 3250-ACCRUE-FOREIGN
               PERFORM 3260-CHECK-NOTICE
               PERFORM 3270-ACCUM-ORDER-TYPE
               PERFORM 3280-EDIT-REF-NO
               PERFORM 3290-ACCUM-TRAN
               PERFORM 3300-ACCUM-BRANCH
               PERFORM 3310-SET-STATUS
               PERFORM 3320-SET-SOURCE
               PERFORM 3330-SET-TYPE
               PERFORM 3340-DESCRIBE-SOURCE
               PERFORM 3350-ASSESS-OVERRIDE
               PERFORM 3360-EDIT-MEMO
               PERFORM 3370-ACCRUE-BUYBACK
               PERFORM 3380-ASSESS-EXEMPT
               PERFORM 3390-SET-TYPE
               PERFORM 3400-ACCRUE-BUYBACK
               PERFORM 3410-SPLIT-MARGIN
               PERFORM 3420-AGE-OPEN
               PERFORM 3430-EDIT-ADDR
               PERFORM 3440-LIMIT-BUYBACK
               PERFORM 3450-SPLIT-USD
               PERFORM 3460-FLAG-USD
               PERFORM 3470-ACCUM-ORDER-TYPE
               PERFORM 3480-LIMIT-USD
               PERFORM 3490-EDIT-CONTACT
               PERFORM 3500-ASSESS-PREFERRED
               PERFORM 3510-DESCRIBE-BRANCH
               PERFORM 3520-DESCRIBE-CURRENCY
               PERFORM 3530-EDIT-MEMO
               PERFORM 3540-EDIT-CONTACT
               PERFORM 3550-SET-TRAN
               PERFORM 3560-PRORATE-USD
               PERFORM 3570-ASSESS-AUTO-RENEW
               PERFORM 3580-LIMIT-BUYBACK
               PERFORM 3590-AGE-EXPIRY
               PERFORM 3600-DESCRIBE-TRAN
               PERFORM 3610-ACCRUE-MARGIN
               PERFORM 3620-ASSESS-AUTO-RENEW
               PERFORM 3630-FLAG-FOREIGN
               PERFORM 3640-SPLIT-FOREIGN
               PERFORM 3650-ACCRUE-USD
               PERFORM 3660-FLAG-MARGIN
               PERFORM 3670-COUNT-EXCEPT
               PERFORM 3680-SELECT-RATE
               PERFORM 3690-LIMIT-USD
               PERFORM 3700-COUNT-PAYMENT
               PERFORM 3710-EDIT-REF-NO
               PERFORM 3720-SET-CHANNEL
               PERFORM 3730-SET-TRAN
               PERFORM 3740-CHECK-LAST-MAINT
               PERFORM 3750-CHECK-LAST-PAY
               PERFORM 3760-SELECT-PENALTY-PCT
               PERFORM 3770-SELECT-PENALTY-PCT
               PERFORM 3780-EDIT-MEMO
               PERFORM 3790-CHECK-NOTICE
               PERFORM 3800-ASSESS-REVERSAL
               PERFORM 3810-SET-TRAN
               PERFORM 3820-PRORATE-FOREIGN
               PERFORM 3830-SPLIT-MARGIN
               PERFORM 3840-ACCRUE-FOREIGN
               PERFORM 3850-EDIT-DESC
               PERFORM 3860-EDIT-REF-NO
               PERFORM 3870-LIMIT-USD
               PERFORM 3880-COUNT-SEQ
               PERFORM 3890-ASSESS-EXEMPT
               PERFORM 3900-LIMIT-ITEM
               PERFORM 3910-SET-CURRENCY
               PERFORM 3920-DESCRIBE-CHANNEL
               PERFORM 3930-ACCUM-SOURCE
               PERFORM 3940-SPLIT-FOREIGN
               PERFORM 3950-SPLIT-USD
               PERFORM 7010-CALL-FNELG
               REWRITE FXN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'FXNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM FXN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E117 NAME REQUIRED
      * E932 WORK AMT 2 NEGATIVE
      * E939 WORK AMT 2 OVER 25000
      * E939 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN FXNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E839 NAME REQUIRED
      * E991 WORK AMT NEGATIVE
      * E794 WORK AMT OVER 5000
      * E284 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E991' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E745 OPERATOR REQUIRED
      * E097 WORK AMT NEGATIVE
      * E615 WORK AMT OVER 5000
      * E476 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E097' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN FXNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3070-FLAG-MARGIN.
           IF FXNT-MARGIN-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-MARGIN.
           IF FXNM-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-MARGIN-AMT * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO FXNM-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-USD.
           IF FXNT-USD-AMT < 10.00
               MOVE 10.00 TO FXNT-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-USD-AMT > 5000.00
               MOVE 5000.00 TO FXNT-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3100-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNT-BUYBACK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNT-BUYBACK-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNM-MARGIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3110-LIMIT-SHIPPING.
           IF FXNM-SHIPPING-YTD < 50.00
               MOVE 50.00 TO FXNM-SHIPPING-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-SHIPPING-YTD > 500.00
               MOVE 500.00 TO FXNM-SHIPPING-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E215 DESC REQUIRED
      * E588 MARGIN AMT NEGATIVE
      * E354 MARGIN AMT OVER 250000
      * E452 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT < 0
                   MOVE 'E588' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT > 250000.00
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E692 MEMO REQUIRED
      * E469 FOREIGN YTD NEGATIVE
      * E683 FOREIGN YTD OVER 25000
      * E746 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-MEMO = SPACES
                   MOVE 'E692' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD < 0
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD > 25000.00
                   MOVE 'E683' TO WS-REJECT-CODE
               WHEN FXNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E746' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNM-BUYBACK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-BUYBACK-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E388 OPERATOR REQUIRED
      * E697 MARGIN YTD NEGATIVE
      * E700 MARGIN YTD OVER 50000
      * E168 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD < 0
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD > 50000.00
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN FXNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E168' TO WS-REJECT-CODE
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
       3170-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3180-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPPING YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3230-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD FXNM-SHIPPING-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-SHIPPING-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNT-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3260-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN YTD BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNM-MARGIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-MARGIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E824 REF NO REQUIRED
      * E238 BUYBACK AMT NEGATIVE
      * E350 BUYBACK AMT OVER 99999
      * E294 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN FXNT-BUYBACK-AMT < 0
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN FXNT-BUYBACK-AMT > 99999.00
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN FXNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNM-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN POST DATE IS PAST
      * ESS -> XDE WHEN POST DATE IS PAST
      * XDE -> XFD WHEN POST DATE IS PAST
      * XFD -> CXH WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN POST DATE IS PAST
      * FS -> HL WHEN POST DATE IS PAST
      * HL -> HS WHEN POST DATE IS PAST
      * HS -> NW WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN POST DATE IS PAST
      * FFU -> JNR WHEN POST DATE IS PAST
      * JNR -> UGS WHEN POST DATE IS PAST
      * UGS -> XUM WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE MARGIN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3350-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNM-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-USD-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO FXNM-USD-AMT
               WHEN FXNT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO FXNM-USD-AMT
               WHEN OTHER
                   COMPUTE FXNM-USD-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-USD-AMT > 0 AND FXNM-USD-AMT < 1.00
               MOVE 1.00 TO FXNM-USD-AMT
           END-IF
           IF FXNM-USD-AMT > 75.00
               MOVE 75.00 TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E970 MEMO REQUIRED
      * E508 BUYBACK AMT NEGATIVE
      * E050 BUYBACK AMT OVER 10000
      * E754 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-MEMO = SPACES
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT < 0
                   MOVE 'E508' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT > 10000.00
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNM-FOREIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE MARGIN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3380-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 7.00 TO FXNM-FOREIGN-AMT
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 3.00 TO FXNM-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-FOREIGN-AMT > 0 AND FXNM-FOREIGN-AMT < 2.00
               MOVE 2.00 TO FXNM-FOREIGN-AMT
           END-IF
           IF FXNM-FOREIGN-AMT > 100.00
               MOVE 100.00 TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN OPEN DATE IS PAST
      * FFU -> JNR WHEN OPEN DATE IS PAST
      * JNR -> UGS WHEN OPEN DATE IS PAST
      * UGS -> XUM WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3400-ACCRUE-BUYBACK.
           IF FXNM-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNT-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNT-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E813 ADDR REQUIRED
      * E756 SHIPPING YTD NEGATIVE
      * E692 SHIPPING YTD OVER 99999
      * E801 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-ADDR = SPACES
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD < 0
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD > 99999.00
                   MOVE 'E692' TO WS-REJECT-CODE
               WHEN FXNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-AMT < 0.00
               MOVE 0.00 TO FXNM-BUYBACK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-AMT > 10000.00
               MOVE 10000.00 TO FXNM-BUYBACK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT USD AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNT-USD-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-USD-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-SHIPPING-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * USD YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3460-FLAG-USD.
           IF FXNM-USD-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-USD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * USD YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3480-LIMIT-USD.
           IF FXNM-USD-YTD < 25.00
               MOVE 25.00 TO FXNM-USD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-YTD > 500.00
               MOVE 500.00 TO FXNM-USD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E781 CONTACT REQUIRED
      * E321 MARGIN AMT NEGATIVE
      * E950 MARGIN AMT OVER 25000
      * E281 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CONTACT = SPACES
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT < 0
                   MOVE 'E321' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT > 25000.00
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE FOREIGN YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3500-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNM-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNM-USD-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 7.00 TO FXNM-USD-AMT
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 12.00 TO FXNM-USD-AMT
               WHEN OTHER
                   COMPUTE FXNM-USD-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-USD-AMT > 0 AND FXNM-USD-AMT < 5.00
               MOVE 5.00 TO FXNM-USD-AMT
           END-IF
           IF FXNM-USD-AMT > 250.00
               MOVE 250.00 TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E601 MEMO REQUIRED
      * E411 MARGIN AMT NEGATIVE
      * E904 MARGIN AMT OVER 5000
      * E263 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-MEMO = SPACES
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT < 0
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT > 5000.00
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E169 CONTACT REQUIRED
      * E771 FOREIGN YTD NEGATIVE
      * E421 FOREIGN YTD OVER 99999
      * E073 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CONTACT = SPACES
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD < 0
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD > 99999.00
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN FXNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN LAST ACT DATE IS PAST
      * BB -> SH WHEN LAST ACT DATE IS PAST
      * SH -> CN WHEN LAST ACT DATE IS PAST
      * CN -> OR WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO USD YTD.
      *----------------------------------------------------------------*
       3560-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-YTD ROUNDED =
                   FXNT-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-USD-YTD
           END-IF
           ADD FXNM-USD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE FOREIGN AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3570-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNM-MARGIN-AMT ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 10.00 TO FXNM-MARGIN-AMT
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 15.00 TO FXNM-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FXNM-MARGIN-AMT ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-MARGIN-AMT > 0 AND FXNM-MARGIN-AMT < 10.00
               MOVE 10.00 TO FXNM-MARGIN-AMT
           END-IF
           IF FXNM-MARGIN-AMT > 25.00
               MOVE 25.00 TO FXNM-MARGIN-AMT
           END-IF
           ADD FXNM-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 10.00
               MOVE 10.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 1000.00
               MOVE 1000.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EXPIRY-DATE > 0 AND FXNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-MARGIN.
           IF FXNT-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-MARGIN-AMT * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3620-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = WS-WORK-AMT-2 *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO FXNM-SHIPPING-AMT
               WHEN FXNT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO FXNM-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = WS-WORK-AMT-2 *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-SHIPPING-AMT > 0 AND FXNM-SHIPPING-AMT < 2.00
               MOVE 2.00 TO FXNM-SHIPPING-AMT
           END-IF
           IF FXNM-SHIPPING-AMT > 250.00
               MOVE 250.00 TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FOREIGN YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3630-FLAG-FOREIGN.
           IF FXNM-FOREIGN-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-FOREIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-USD.
           IF FXNM-USD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-USD-YTD * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNM-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3660-FLAG-MARGIN.
           IF FXNM-MARGIN-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-SHIPPING-SCHED-LOW(WS-TX)
                   MOVE WS-SHIPPING-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-USD.
           IF FXNM-USD-AMT < 0.00
               MOVE 0.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 1000.00
               MOVE 1000.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E567 REF NO REQUIRED
      * E122 BUYBACK YTD NEGATIVE
      * E880 BUYBACK YTD OVER 99999
      * E475 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD < 0
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD > 99999.00
                   MOVE 'E880' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN OPEN DATE IS PAST
      * BB -> SH WHEN OPEN DATE IS PAST
      * SH -> CN WHEN OPEN DATE IS PAST
      * CN -> OR WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3730-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD TIER TIER FOR THE USD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-USD-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNM-USD-YTD NOT < WS-USD-TIER-LOW(WS-TX)
                   MOVE WS-USD-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING SCHED TIER FOR THE MARGIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-SCHED-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF FXNM-MARGIN-YTD NOT < WS-SHIPPING-SCHED-LOW(WS-TX)
                   MOVE WS-SHIPPING-SCHED-RATE(WS-TX) TO
                       FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E107 MEMO REQUIRED
      * E355 WORK AMT 2 NEGATIVE
      * E217 WORK AMT 2 OVER 10000
      * E251 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-MEMO = SPACES
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE FOREIGN YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3800-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNM-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-MARGIN-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-RATE / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 15.00 TO FXNM-MARGIN-AMT
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 20.00 TO FXNM-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FXNM-MARGIN-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-MARGIN-AMT > 0 AND FXNM-MARGIN-AMT < 10.00
               MOVE 10.00 TO FXNM-MARGIN-AMT
           END-IF
           IF FXNM-MARGIN-AMT > 75.00
               MOVE 75.00 TO FXNM-MARGIN-AMT
           END-IF
           ADD FXNM-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN CLOSE DATE IS PAST
      * BB -> SH WHEN CLOSE DATE IS PAST
      * SH -> CN WHEN CLOSE DATE IS PAST
      * CN -> OR WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FOREIGN YTD.
      *----------------------------------------------------------------*
       3820-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-YTD ROUNDED =
                   FXNM-SHIPPING-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNT-MARGIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNT-MARGIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNM-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNM-MARGIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E183 DESC REQUIRED
      * E620 WORK AMT NEGATIVE
      * E157 WORK AMT OVER 5000
      * E750 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E183' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E157' TO WS-REJECT-CODE
               WHEN FXNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E920 REF NO REQUIRED
      * E808 BUYBACK AMT NEGATIVE
      * E038 BUYBACK AMT OVER 25000
      * E811 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E920' TO WS-REJECT-CODE
               WHEN FXNT-BUYBACK-AMT < 0
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN FXNT-BUYBACK-AMT > 25000.00
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E811' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3870-LIMIT-USD.
           IF FXNT-USD-AMT < 10.00
               MOVE 10.00 TO FXNT-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-USD-AMT > 500.00
               MOVE 500.00 TO FXNT-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-SEQ.
           IF FXNT-SEQ < 9999999
               ADD 1 TO FXNT-SEQ
           END-IF
           IF FXNT-SEQ NOT < 10
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE MARGIN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3890-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNT-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 20.00 TO FXNT-USD-AMT
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 12.00 TO FXNT-USD-AMT
               WHEN OTHER
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-USD-AMT > 0 AND FXNT-USD-AMT < 2.00
               MOVE 2.00 TO FXNT-USD-AMT
           END-IF
           IF FXNT-USD-AMT > 35.00
               MOVE 35.00 TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN OPEN DATE IS PAST
      * E -> G WHEN OPEN DATE IS PAST
      * G -> P WHEN OPEN DATE IS PAST
      * P -> W WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNT-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE FXNT-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-USD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE FXN FIELDS TO FNELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-FNELG.
           INITIALIZE FNELG-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNELG-FOREIGN-AMT
           MOVE FXNM-SHIPPING-AMT
             TO FNELG-SHIPPING-AMT
           MOVE FXNM-MARGIN-YTD
             TO FNELG-MARGIN-YTD
           MOVE FXNM-USD-AMT
             TO FNELG-USD-AMT
           MOVE FXNM-MARGIN-AMT
             TO FNELG-MARGIN-AMT
           MOVE FXNM-BUYBACK-AMT
             TO FNELG-BUYBACK-AMT
           MOVE FXNM-NOTICE-DATE
             TO FNELG-NOTICE-DATE
           MOVE FXNM-LAST-MAINT-DATE
             TO FNELG-LAST-MAINT-DATE
           MOVE FXNM-OPEN-DATE
             TO FNELG-OPEN-DATE
           MOVE FXNM-LAST-PAY-DATE
             TO FNELG-LAST-PAY-DATE
           MOVE FXNM-STATUS-CD
             TO FNELG-STATUS-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNELG-ORDER-TYPE-CD
           MOVE FXNM-BRANCH-CD
             TO FNELG-BRANCH-CD
           MOVE FXNM-SOURCE-CD
             TO FNELG-SOURCE-CD
           MOVE FXNM-OVERRIDE-FLAG
             TO FNELG-OVERRIDE-FLAG
           MOVE FXNM-PREFERRED-FLAG
             TO FNELG-PREFERRED-FLAG
           MOVE FXNM-REVIEW-FLAG
             TO FNELG-REVIEW-FLAG
           MOVE FXNM-PAYMENT-CNT
             TO FNELG-PAYMENT-CNT
           MOVE FXNM-CYCLE-CNT
             TO FNELG-CYCLE-CNT
           MOVE FXNM-RATE
             TO FNELG-RATE
           MOVE FXNM-PENALTY-PCT
             TO FNELG-PENALTY-PCT
           MOVE FXNM-DESC
             TO FNELG-DESC
           MOVE FXNM-ADDR
             TO FNELG-ADDR
           CALL 'FNELG' USING FNELG-PARMS
           EVALUATE FNELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNELG-USD-AMT
                     TO FXNM-USD-AMT
                   MOVE FNELG-MARGIN-AMT
                     TO FXNM-MARGIN-AMT
                   MOVE FNELG-PREFERRED-FLAG
                     TO FXNM-PREFERRED-FLAG
                   MOVE FNELG-REVIEW-FLAG
                     TO FXNM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-FNELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE FXNT-KEY TO RD-KEY
           MOVE FXNT-TRAN-CD TO RD-CODE
           MOVE FXNT-MARGIN-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE FXNT-KEY TO RD-KEY
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
           DISPLAY 'FNNEXCP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'FNNEXCP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
