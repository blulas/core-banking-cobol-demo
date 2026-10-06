      *================================================================*
      * PROGRAM  : DRPSWEP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DIVIDEND REINVESTMENT - SWEEP. READS THE DRP        *
      *            TRANSACTIONS, UPDATES THE DRP MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BANK HOLDING COMPANY STOCK      *
      *            DIVIDEND REINVESTMENT PLAN                          *
      * INPUT    : DRPMST (KSDS), DRPTRN, BANKCTL                      *
      * OUTPUT   : DRPOUT, DRPSWRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, DRELG                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-01-26 JLM  ORIGINAL                                       *
      * 1984-06-08 LMB  CASH-PAY CALCULATION CORRECTED REQ0924         *
      * 1984-11-11 DPS  SHARES-AMT TABLE EXPANDED PRB8716              *
      * 1985-08-21 PQR  WITHHOLD LIMIT REVISED PRB2855                 *
      * 1993-07-28 DPS  AUDIT FINDING - FEE CONTROL TOTALS PRB4268     *
      * 1996-08-10 MKL  AUDIT FINDING - DIVIDEND CONTROL TOTALS INC3193*
      * 1999-03-24 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-01-11 EWS  AUDIT FINDING - FEE CONTROL TOTALS REQ1457     *
      * 2004-08-22 JLM  FEE CUTOFF MOVED REQ5253                       *
      * 2004-11-12 RJT  WITHHOLD STATUS CODES ADDED PRB8150            *
      * 2005-03-03 EWS  SHARES-AMT RERUN PROTECTION CHG7780            *
      * 2005-09-22 DPS  SHARES-AMT FEE SCHEDULE UPDATE CHG4928         *
      * 2008-01-25 TWH  SHARES-AMT TABLE EXPANDED INC8126              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRPSWEP.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "DRPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DRPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "DRPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "DRPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "DRPSWRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY DRPMST.
       FD  TRAN-FILE.
       COPY DRPTRN.
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
      * WS-DIVIDEND-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DIVIDEND-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000696000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000630000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000871000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001200000000083000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003700000000609500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013700000000355500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023700000000743500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023950000000668000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024050000000552000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025050000000111000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030050000000433000'.
       01  WS-DIVIDEND-TIER-TBL REDEFINES WS-DIVIDEND-TIER-VALUES.
           05  WS-DIVIDEND-TIER-ROW OCCURS 11 TIMES.
               10  WS-DIVIDEND-TIER-LOW    PIC 9(09)V99.
               10  WS-DIVIDEND-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000791500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000457000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000188500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000445000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008050000000368000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000036000'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 6 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REINVEST-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REINVEST-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000123000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000346000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000173000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000350500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025050000000170500'.
       01  WS-REINVEST-BAND-TBL REDEFINES WS-REINVEST-BAND-VALUES.
           05  WS-REINVEST-BAND-ROW OCCURS 5 TIMES.
               10  WS-REINVEST-BAND-LOW    PIC 9(09)V99.
               10  WS-REINVEST-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WITHHOLD-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WITHHOLD-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000157000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000723000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000627000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000203500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000868500'.
       01  WS-WITHHOLD-TIER-TBL REDEFINES WS-WITHHOLD-TIER-VALUES.
           05  WS-WITHHOLD-TIER-ROW OCCURS 5 TIMES.
               10  WS-WITHHOLD-TIER-LOW    PIC 9(09)V99.
               10  WS-WITHHOLD-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  DRPSWEP  DIVIDEND REINVESTMENT'.
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
       COPY DRELGP.

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
               MOVE 'DRPMST' TO ABND-FILE-NAME
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
           MOVE DRPT-KEY TO DRPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-COUNT-NOTICE
               PERFORM 3010-SET-CYCLE
               PERFORM 3020-SET-CYCLE
               PERFORM 3030-COUNT-NOTICE
               PERFORM 3040-ACCUM-CYCLE
               PERFORM 3050-COUNT-ITEM
               PERFORM 3060-DESCRIBE-CHANNEL
               PERFORM 3070-PRORATE-REINVEST
               PERFORM 3080-LIMIT-CASH-PAY
               PERFORM 3090-ACCRUE-REINVEST
               PERFORM 3100-DESCRIBE-CHANNEL
               PERFORM 3110-DESCRIBE-SOURCE
               PERFORM 3120-PRORATE-CASH-PAY
               PERFORM 3130-FLAG-FEE
               PERFORM 3140-SPLIT-SHARES-AMT
               PERFORM 3150-SELECT-DISCOUNT
               PERFORM 3160-SELECT-RATE
               PERFORM 3170-SPLIT-WITHHOLD
               PERFORM 3180-FLAG-REINVEST
               PERFORM 3190-CHECK-MATURITY
               PERFORM 3200-ACCRUE-REINVEST
               PERFORM 3210-ACCRUE-CASH-PAY
               PERFORM 3220-COUNT-NOTICE
               PERFORM 3230-DESCRIBE-REASON
               PERFORM 3240-EDIT-MEMO
               PERFORM 3250-COUNT-SEQ
               PERFORM 3260-SET-CYCLE
               PERFORM 3270-CHECK-LAST-MAINT
               PERFORM 3280-LIMIT-REINVEST
               PERFORM 3290-DESCRIBE-CYCLE
               PERFORM 3300-SELECT-PENALTY-PCT
               PERFORM 3310-LIMIT-FEE
               PERFORM 3320-CHECK-EFFECTIVE
               PERFORM 3330-LIMIT-CASH-PAY
               PERFORM 3340-PRORATE-FEE
               PERFORM 3350-LIMIT-SHARES-AMT
               PERFORM 3360-ACCRUE-DIVIDEND
               PERFORM 3370-AGE-EFF
               PERFORM 3380-CHECK-EFFECTIVE
               PERFORM 3390-AGE-EFFECTIVE
               PERFORM 3400-DESCRIBE-SOURCE
               PERFORM 3410-SELECT-PENALTY-PCT
               PERFORM 3420-SPLIT-FEE
               PERFORM 3430-FLAG-REINVEST
               PERFORM 3440-CHECK-POST
               PERFORM 3450-SPLIT-DIVIDEND
               PERFORM 3460-PRORATE-DIVIDEND
               PERFORM 3470-AGE-NEXT-DUE
               PERFORM 3480-ASSESS-OVERRIDE
               PERFORM 3490-ASSESS-SUSPEND
               PERFORM 3500-FLAG-ITEM
               PERFORM 3510-PRORATE-REINVEST
               PERFORM 3520-COUNT-ITEM
               PERFORM 3530-ACCUM-CHANNEL
               PERFORM 3540-SELECT-PENALTY-PCT
               PERFORM 3550-AGE-EFFECTIVE
               PERFORM 3560-FLAG-SHARES-AMT
               PERFORM 3570-DESCRIBE-CHANNEL
               PERFORM 3580-FLAG-WITHHOLD
               PERFORM 3590-DESCRIBE-REASON
               PERFORM 3600-CHECK-OPEN
               PERFORM 3610-AGE-MATURITY
               PERFORM 3620-ACCUM-CYCLE
               PERFORM 3630-DESCRIBE-CYCLE
               PERFORM 3640-SELECT-DISCOUNT
               PERFORM 3650-AGE-LAST-ACT
               PERFORM 3660-ACCRUE-REINVEST
               PERFORM 3670-PRORATE-FEE
               PERFORM 3680-ASSESS-OVERRIDE
               PERFORM 3690-ACCRUE-DIVIDEND
               PERFORM 3700-LIMIT-WITHHOLD
               PERFORM 3710-AGE-EFF
               PERFORM 3720-EDIT-OPERATOR
               PERFORM 3730-ASSESS-REVERSAL
               PERFORM 3740-SELECT-PENALTY-PCT
               PERFORM 3750-AGE-LAST-MAINT
               PERFORM 3760-SPLIT-REINVEST
               PERFORM 3770-SPLIT-REINVEST
               PERFORM 3780-CHECK-NEXT-REVIEW
               PERFORM 3790-SET-SOURCE
               PERFORM 3800-CHECK-EFFECTIVE
               PERFORM 3810-ASSESS-REVERSAL
               PERFORM 3820-SET-CHANNEL
               PERFORM 3830-COUNT-EXCEPT
               PERFORM 3840-ASSESS-STOP
               PERFORM 3850-CHECK-NEXT-DUE
               PERFORM 3860-DESCRIBE-CYCLE
               PERFORM 3870-ACCRUE-WITHHOLD
               PERFORM 3880-LIMIT-REINVEST
               PERFORM 3890-ACCUM-CHANNEL
               PERFORM 3900-ACCRUE-FEE
               PERFORM 3910-ACCRUE-FEE
               PERFORM 3920-SELECT-PENALTY-PCT
               PERFORM 3930-EDIT-MEMO
               PERFORM 3940-COUNT-SEQ
               PERFORM 3950-ACCUM-SOURCE
               PERFORM 3960-CHECK-MATURITY
               PERFORM 3970-COUNT-ITEM
               PERFORM 3980-DESCRIBE-TRAN
               PERFORM 3990-FLAG-REINVEST
               PERFORM 4000-PRORATE-DIVIDEND
               PERFORM 4010-LIMIT-WITHHOLD
               PERFORM 4020-EDIT-DESC
               PERFORM 4030-PRORATE-DIVIDEND
               PERFORM 4040-SET-CHANNEL
               PERFORM 4050-AGE-OPEN
               PERFORM 4060-FLAG-REINVEST
               PERFORM 4070-AGE-LAST-MAINT
               PERFORM 4080-SPLIT-FEE
               PERFORM 4090-FLAG-REINVEST
               PERFORM 4100-EDIT-CUST-ID
               PERFORM 4110-SELECT-DISCOUNT
               PERFORM 4120-PRORATE-REINVEST
               PERFORM 4130-CHECK-POST
               PERFORM 4140-COUNT-PAYMENT
               PERFORM 4150-CHECK-NOTICE
               PERFORM 4160-SET-PLAN-OPTION
               PERFORM 4170-ACCRUE-DIVIDEND
               PERFORM 4180-ASSESS-EXEMPT
               PERFORM 4190-ASSESS-SUSPEND
               PERFORM 4200-FLAG-SHARES-AMT
               PERFORM 4210-SET-PLAN-OPTION
               PERFORM 4220-SPLIT-REINVEST
               PERFORM 4230-CHECK-LAST-PAY
               PERFORM 4240-COUNT-ITEM
               PERFORM 4250-SPLIT-FEE
               PERFORM 4260-SPLIT-FEE
               PERFORM 4270-AGE-LAST-PAY
               PERFORM 4280-SPLIT-DIVIDEND
               PERFORM 4290-EDIT-CUST-ID
               PERFORM 4300-EDIT-OPERATOR
               PERFORM 4310-SPLIT-CASH-PAY
               PERFORM 7020-CALL-DRELG
               REWRITE DRP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'DRPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM DRP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN OPEN DATE IS PAST
      * FB -> PC WHEN OPEN DATE IS PAST
      * PC -> WY WHEN OPEN DATE IS PAST
      * WY -> XD WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN EFFECTIVE DATE IS PAST
      * FB -> PC WHEN EFFECTIVE DATE IS PAST
      * PC -> WY WHEN EFFECTIVE DATE IS PAST
      * WY -> XD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 6
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       3070-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-AMT ROUNDED =
                   DRPM-SHARES-AMT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-YTD < 25.00
               MOVE 25.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-YTD > 10000.00
               MOVE 10000.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-REINVEST.
           IF DRPT-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-REINVEST-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH PAY AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-CASH-PAY-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-CASH-PAY-AMT
           END-IF
           ADD DRPM-CASH-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3130-FLAG-FEE.
           IF DRPM-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF DRPT-DIVIDEND-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST BAND TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRPT-REINVEST-AMT NOT < WS-REINVEST-BAND-LOW(WS-TX)
                   MOVE WS-REINVEST-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRPM-WITHHOLD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-WITHHOLD-YTD - (WS-WORK-AMT * 2)
           COMPUTE DRPT-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3180-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-REINVEST.
           IF DRPM-REINVEST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-YTD * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-YTD * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'G'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E532 MEMO REQUIRED
      * E721 DIVIDEND YTD NEGATIVE
      * E159 DIVIDEND YTD OVER 5000
      * E292 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD < 0
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD > 5000.00
                   MOVE 'E159' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-SEQ.
           IF DRPT-SEQ < 9999999
               ADD 1 TO DRPT-SEQ
           END-IF
           IF DRPT-SEQ NOT < 10
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN LAST ACT DATE IS PAST
      * FB -> PC WHEN LAST ACT DATE IS PAST
      * PC -> WY WHEN LAST ACT DATE IS PAST
      * WY -> XD WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3280-LIMIT-REINVEST.
           IF DRPM-REINVEST-AMT < 50.00
               MOVE 50.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-AMT > 500.00
               MOVE 500.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REINVEST BAND TIER FOR THE SHARES AMT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REINVEST-BAND-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRPM-SHARES-AMT-YTD NOT < WS-REINVEST-BAND-LOW(WS-TX)
                   MOVE WS-REINVEST-BAND-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3310-LIMIT-FEE.
           IF DRPM-FEE-YTD < 0.00
               MOVE 0.00 TO DRPM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-FEE-YTD > 1000.00
               MOVE 1000.00 TO DRPM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3330-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-YTD < 100.00
               MOVE 100.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-YTD > 500.00
               MOVE 500.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3340-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-YTD ROUNDED =
                   DRPT-SHARES-AMT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHARES AMT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3350-LIMIT-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD < 50.00
               MOVE 50.00 TO DRPM-SHARES-AMT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-SHARES-AMT-YTD > 500.00
               MOVE 500.00 TO DRPM-SHARES-AMT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD TIER TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRPM-DIVIDEND-AMT NOT < WS-WITHHOLD-TIER-LOW(WS-TX)
                   MOVE WS-WITHHOLD-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE DRPM-CASH-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3430-FLAG-REINVEST.
           IF DRPM-REINVEST-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DIVIDEND AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = DRPM-DIVIDEND-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-DIVIDEND-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-SHARES-AMT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3460-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                   DRPT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3480-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPT-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-RATE / 200
               WHEN DRPM-CHANNEL-CD = 'RM'
                   MOVE 20.00 TO DRPT-SHARES-AMT-AMT
               WHEN DRPM-CHANNEL-CD = 'VP'
                   MOVE 7.00 TO DRPT-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPT-SHARES-AMT-AMT > 0 AND DRPT-SHARES-AMT-AMT < 5.00
               MOVE 5.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           IF DRPT-SHARES-AMT-AMT > 50.00
               MOVE 50.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           ADD DRPT-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND YTD FEE ON THE DIVIDEND AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3490-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-YTD ROUNDED = DRPT-DIVIDEND-AMT
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 3.00 TO DRPM-DIVIDEND-YTD
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 10.00 TO DRPM-DIVIDEND-YTD
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-YTD ROUNDED = DRPT-DIVIDEND-AMT
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-YTD > 0 AND DRPM-DIVIDEND-YTD < 10.00
               MOVE 10.00 TO DRPM-DIVIDEND-YTD
           END-IF
           IF DRPM-DIVIDEND-YTD > 25.00
               MOVE 25.00 TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3500-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHARES AMT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-SHARES-AMT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3580-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'J'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND TIER TIER FOR THE CASH PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-CASH-PAY-YTD NOT < WS-DIVIDEND-TIER-LOW(WS-TX)
                   MOVE WS-DIVIDEND-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-REINVEST.
           IF DRPM-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-AMT * DRPM-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-AMT ROUNDED =
                   DRPT-SHARES-AMT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-FEE-AMT
           END-IF
           ADD DRPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST FEE ON THE CASH PAY AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3680-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-REINVEST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-REINVEST-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO DRPM-REINVEST-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO DRPM-REINVEST-AMT
               WHEN OTHER
                   COMPUTE DRPM-REINVEST-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-REINVEST-AMT > 0 AND DRPM-REINVEST-AMT < 5.00
               MOVE 5.00 TO DRPM-REINVEST-AMT
           END-IF
           IF DRPM-REINVEST-AMT > 25.00
               MOVE 25.00 TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-AMT * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-WITHHOLD.
           IF DRPM-WITHHOLD-AMT < 100.00
               MOVE 100.00 TO DRPM-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-WITHHOLD-AMT > 50000.00
               MOVE 50000.00 TO DRPM-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E452 OPERATOR REQUIRED
      * E604 FEE AMT NEGATIVE
      * E442 FEE AMT OVER 50000
      * E478 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN DRPT-FEE-AMT < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN DRPT-FEE-AMT > 50000.00
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN DRPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE DIVIDEND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3730-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-DIVIDEND-AMT
                       * DRPM-RATE / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-DIVIDEND-AMT
                       * DRPM-RATE / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 10.00
               MOVE 10.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 25.00
               MOVE 25.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD TIER TIER FOR THE REINVEST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRPM-REINVEST-YTD NOT < WS-WITHHOLD-TIER-LOW(WS-TX)
                   MOVE WS-WITHHOLD-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-AMT - (WS-WORK-AMT * 4)
           COMPUTE DRPT-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN POST DATE IS PAST
      * FL -> NK WHEN POST DATE IS PAST
      * NK -> YC WHEN POST DATE IS PAST
      * YC -> BT WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE SHARES AMT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3810-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-FEE-YTD ROUNDED = DRPM-SHARES-AMT-YTD *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 15.00 TO DRPM-FEE-YTD
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 3.00 TO DRPM-FEE-YTD
               WHEN OTHER
                   COMPUTE DRPM-FEE-YTD ROUNDED = DRPM-SHARES-AMT-YTD *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-FEE-YTD > 0 AND DRPM-FEE-YTD < 1.00
               MOVE 1.00 TO DRPM-FEE-YTD
           END-IF
           IF DRPM-FEE-YTD > 35.00
               MOVE 35.00 TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN EFF DATE IS PAST
      * VP -> YS WHEN EFF DATE IS PAST
      * YS -> RM WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD FEE ON THE FEE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3840-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPM-WITHHOLD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   COMPUTE DRPM-WITHHOLD-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 10.00 TO DRPM-WITHHOLD-AMT
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 10.00 TO DRPM-WITHHOLD-AMT
               WHEN OTHER
                   COMPUTE DRPM-WITHHOLD-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-WITHHOLD-AMT > 0 AND DRPM-WITHHOLD-AMT < 10.00
               MOVE 10.00 TO DRPM-WITHHOLD-AMT
           END-IF
           IF DRPM-WITHHOLD-AMT > 75.00
               MOVE 75.00 TO DRPM-WITHHOLD-AMT
           END-IF
           ADD DRPM-WITHHOLD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-WITHHOLD.
           IF DRPM-WITHHOLD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-WITHHOLD-YTD * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPT-DIVIDEND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-REINVEST.
           IF DRPM-REINVEST-YTD < 25.00
               MOVE 25.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-YTD > 2500.00
               MOVE 2500.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-FEE.
           IF DRPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-REINVEST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-FEE.
           IF DRPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-FEE-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-REINVEST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF DRPT-FEE-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E660 MEMO REQUIRED
      * E470 REINVEST AMT NEGATIVE
      * E690 REINVEST AMT OVER 1000000
      * E190 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT < 0
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT > 1000000.00
                   MOVE 'E690' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-SEQ.
           IF DRPT-SEQ < 9999999
               ADD 1 TO DRPT-SEQ
           END-IF
           IF DRPT-SEQ NOT < 3
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-SOURCE-CD
                   ADD DRPM-SHARES-AMT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-SHARES-AMT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RI'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3990-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND YTD.
      *----------------------------------------------------------------*
       4000-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-YTD ROUNDED =
                   DRPM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-WITHHOLD.
           IF DRPM-WITHHOLD-AMT < 100.00
               MOVE 100.00 TO DRPM-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-WITHHOLD-AMT > 5000.00
               MOVE 5000.00 TO DRPM-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E850 DESC REQUIRED
      * E953 WORK AMT 2 NEGATIVE
      * E392 WORK AMT 2 OVER 10000
      * E840 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN DRPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4030-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4060-FLAG-REINVEST.
           IF DRPT-REINVEST-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPT-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-REINVEST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4090-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E137 CUST ID REQUIRED
      * E285 CASH PAY AMT NEGATIVE
      * E703 CASH PAY AMT OVER 5000
      * E657 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-AMT < 0
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-AMT > 5000.00
                   MOVE 'E703' TO WS-REJECT-CODE
               WHEN DRPT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND TIER TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-WITHHOLD-AMT NOT < WS-DIVIDEND-TIER-LOW(WS-TX)
                   MOVE WS-DIVIDEND-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4120-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   DRPT-REINVEST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN OPEN DATE IS PAST
      * R -> T WHEN OPEN DATE IS PAST
      * T -> Z WHEN OPEN DATE IS PAST
      * Z -> E WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-AMT * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4180-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED = WS-WORK-AMT-2 *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 20.00 TO DRPM-SHARES-AMT-YTD
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 7.00 TO DRPM-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED = WS-WORK-AMT-2 *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-YTD > 0 AND DRPM-SHARES-AMT-YTD < 2.00
               MOVE 2.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           IF DRPM-SHARES-AMT-YTD > 75.00
               MOVE 75.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD FEE ON THE DIVIDEND AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4190-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-WITHHOLD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRPM-WITHHOLD-AMT ROUNDED = DRPT-DIVIDEND-AMT
                       * DRPM-RATE / 200
               WHEN DRPM-CYCLE-CD = 'DE'
                   MOVE 15.00 TO DRPM-WITHHOLD-AMT
               WHEN DRPM-CYCLE-CD = 'FB'
                   MOVE 12.00 TO DRPM-WITHHOLD-AMT
               WHEN OTHER
                   COMPUTE DRPM-WITHHOLD-AMT ROUNDED = DRPT-DIVIDEND-AMT
                       * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-WITHHOLD-AMT > 0 AND DRPM-WITHHOLD-AMT < 10.00
               MOVE 10.00 TO DRPM-WITHHOLD-AMT
           END-IF
           IF DRPM-WITHHOLD-AMT > 35.00
               MOVE 35.00 TO DRPM-WITHHOLD-AMT
           END-IF
           ADD DRPM-WITHHOLD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHARES AMT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4200-FLAG-SHARES-AMT.
           IF DRPM-SHARES-AMT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-SHARES-AMT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN LAST ACT DATE IS PAST
      * R -> T WHEN LAST ACT DATE IS PAST
      * T -> Z WHEN LAST ACT DATE IS PAST
      * Z -> E WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT * 4)
           COMPUTE DRPT-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE DRPM-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4260-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DIVIDEND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = DRPM-DIVIDEND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-DIVIDEND-YTD - (WS-WORK-AMT * 4)
           COMPUTE DRPM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E918 CUST ID REQUIRED
      * E805 SHARES AMT AMT NEGATIVE
      * E153 SHARES AMT AMT OVER 250000
      * E510 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-AMT < 0
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-AMT > 250000.00
                   MOVE 'E153' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E074 OPERATOR REQUIRED
      * E324 CASH PAY YTD NEGATIVE
      * E724 CASH PAY YTD OVER 10000
      * E096 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD < 0
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD > 10000.00
                   MOVE 'E724' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-REINVEST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-DRELG.
           INITIALIZE DRELG-PARMS
           MOVE DRPM-REINVEST-AMT
             TO DRELG-REINVEST-AMT
           MOVE DRPM-WITHHOLD-AMT
             TO DRELG-WITHHOLD-AMT
           MOVE DRPM-FEE-YTD
             TO DRELG-FEE-YTD
           MOVE DRPM-SHARES-AMT-AMT
             TO DRELG-SHARES-AMT-AMT
           MOVE DRPM-CASH-PAY-AMT
             TO DRELG-CASH-PAY-AMT
           MOVE DRPM-CASH-PAY-YTD
             TO DRELG-CASH-PAY-YTD
           MOVE DRPM-LAST-MAINT-DATE
             TO DRELG-LAST-MAINT-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRELG-MATURITY-DATE
           MOVE DRPM-OPEN-DATE
             TO DRELG-OPEN-DATE
           MOVE DRPM-EFFECTIVE-DATE
             TO DRELG-EFFECTIVE-DATE
           MOVE DRPM-SOURCE-CD
             TO DRELG-SOURCE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRELG-PLAN-OPTION-CD
           MOVE DRPM-CYCLE-CD
             TO DRELG-CYCLE-CD
           MOVE DRPM-CHANNEL-CD
             TO DRELG-CHANNEL-CD
           MOVE DRPM-STOP-FLAG
             TO DRELG-STOP-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRELG-SUSPEND-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRELG-PREFERRED-FLAG
           MOVE DRPM-EXCEPT-CNT
             TO DRELG-EXCEPT-CNT
           MOVE DRPM-ITEM-CNT
             TO DRELG-ITEM-CNT
           MOVE DRPM-DISCOUNT
             TO DRELG-DISCOUNT
           MOVE DRPM-RATE
             TO DRELG-RATE
           MOVE DRPM-REF-NO
             TO DRELG-REF-NO
           MOVE DRPM-CUST-ID
             TO DRELG-CUST-ID
           CALL 'DRELG' USING DRELG-PARMS
           EVALUATE DRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRELG-FEE-YTD
                     TO DRPM-FEE-YTD
                   MOVE DRELG-SHARES-AMT-AMT
                     TO DRPM-SHARES-AMT-AMT
                   MOVE DRELG-CASH-PAY-AMT
                     TO DRPM-CASH-PAY-AMT
                   MOVE DRELG-CASH-PAY-YTD
                     TO DRPM-CASH-PAY-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-DRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE DRPT-KEY TO RD-KEY
           MOVE DRPT-TRAN-CD TO RD-CODE
           MOVE DRPT-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE DRPT-KEY TO RD-KEY
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
           DISPLAY 'DRPSWEP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'DRPSWEP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
