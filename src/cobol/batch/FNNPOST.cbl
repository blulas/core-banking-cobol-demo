      *================================================================*
      * PROGRAM  : FNNPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : FOREIGN BANKNOTES - DAILY POSTING. READS THE FXN    *
      *            TRANSACTIONS, UPDATES THE FXN MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FOREIGN CURRENCY BANKNOTE       *
      *            ORDERS, BUYBACK AND MARGIN                          *
      * INPUT    : FXNMST (KSDS), FXNTRN, BANKCTL                      *
      * OUTPUT   : FXNOUT, FNNPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, FNELG, FNTIER, FNSTAT   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-24 SKN  ORIGINAL                                       *
      * 1999-11-01 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-26 PQR  USD FEE SCHEDULE UPDATE REQ8856                *
      * 2003-06-26 TWH  NEW SHIPPING REPORT COLUMNS PRB4627            *
      * 2005-12-12 EWS  FOREIGN NOTICE TEXT PER LEGAL CHG4132          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNNPOST.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "FNNPORPT"
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
                                           VALUE 25.000000.
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
      * WS-BUYBACK-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000524000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000036000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000535000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000710500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015350000000684500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000367500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025600000000264000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025650000000332500'.
       01  WS-BUYBACK-SCHED-TBL REDEFINES WS-BUYBACK-SCHED-VALUES.
           05  WS-BUYBACK-SCHED-ROW OCCURS 8 TIMES.
               10  WS-BUYBACK-SCHED-LOW    PIC 9(09)V99.
               10  WS-BUYBACK-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FOREIGN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FOREIGN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000446000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000128500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000734000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000505000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000703500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000458000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000837500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014150000000061000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019150000000075500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019250000000589000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019300000000762500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029300000000890500'.
       01  WS-FOREIGN-TIER-TBL REDEFINES WS-FOREIGN-TIER-VALUES.
           05  WS-FOREIGN-TIER-ROW OCCURS 12 TIMES.
               10  WS-FOREIGN-TIER-LOW     PIC 9(09)V99.
               10  WS-FOREIGN-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MARGIN-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MARGIN-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000172000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000014500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000585500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015350000000687500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025350000000759000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000757500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026850000000119500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026950000000145500'.
       01  WS-MARGIN-GRID-TBL REDEFINES WS-MARGIN-GRID-VALUES.
           05  WS-MARGIN-GRID-ROW OCCURS 9 TIMES.
               10  WS-MARGIN-GRID-LOW      PIC 9(09)V99.
               10  WS-MARGIN-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHIPPING-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPPING-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000639000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000774000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000683000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000200500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000332000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000499000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000402500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000618000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000106000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016600000000379500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017100000000875500'.
       01  WS-SHIPPING-TIER-TBL REDEFINES WS-SHIPPING-TIER-VALUES.
           05  WS-SHIPPING-TIER-ROW OCCURS 11 TIMES.
               10  WS-SHIPPING-TIER-LOW    PIC 9(09)V99.
               10  WS-SHIPPING-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  FNNPOST  FOREIGN BANKNOTES'.
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
       COPY FNTIERP.
       COPY FNSTATP.

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
               PERFORM 3000-COUNT-NOTICE
               PERFORM 3010-DESCRIBE-STATUS
               PERFORM 3020-FLAG-USD
               PERFORM 3030-SPLIT-USD
               PERFORM 3040-PRORATE-FOREIGN
               PERFORM 3050-SET-CHANNEL
               PERFORM 3060-EDIT-CUST-ID
               PERFORM 3070-ACCUM-ORDER-TYPE
               PERFORM 3080-SPLIT-BUYBACK
               PERFORM 3090-AGE-EFF
               PERFORM 3100-AGE-POST
               PERFORM 3110-ASSESS-NOTICE-SENT
               PERFORM 3120-DESCRIBE-TRAN
               PERFORM 3130-EDIT-CUST-ID
               PERFORM 3140-LIMIT-FOREIGN
               PERFORM 3150-ACCRUE-FOREIGN
               PERFORM 3160-FLAG-SHIPPING
               PERFORM 3170-CHECK-EXPIRY
               PERFORM 3180-FLAG-FOREIGN
               PERFORM 3190-SET-SOURCE
               PERFORM 3200-SET-SOURCE
               PERFORM 3210-SELECT-PENALTY-PCT
               PERFORM 3220-LIMIT-MARGIN
               PERFORM 3230-CHECK-OPEN
               PERFORM 3240-ACCRUE-FOREIGN
               PERFORM 3250-EDIT-OPERATOR
               PERFORM 3260-COUNT-EXCEPT
               PERFORM 3270-PRORATE-USD
               PERFORM 3280-ACCRUE-FOREIGN
               PERFORM 3290-ACCUM-CHANNEL
               PERFORM 3300-AGE-LAST-PAY
               PERFORM 3310-SPLIT-USD
               PERFORM 3320-SELECT-PENALTY-PCT
               PERFORM 3330-LIMIT-FOREIGN
               PERFORM 3340-DESCRIBE-STATUS
               PERFORM 3350-LIMIT-BUYBACK
               PERFORM 3360-SELECT-RATE
               PERFORM 3370-COUNT-NOTICE
               PERFORM 3380-SPLIT-FOREIGN
               PERFORM 3390-EDIT-REF-NO
               PERFORM 3400-FLAG-MARGIN
               PERFORM 3410-AGE-EFF
               PERFORM 3420-SET-ORDER-TYPE
               PERFORM 3430-ACCRUE-FOREIGN
               PERFORM 3440-SET-TRAN
               PERFORM 3450-SET-ORDER-TYPE
               PERFORM 3460-PRORATE-BUYBACK
               PERFORM 3470-PRORATE-MARGIN
               PERFORM 3480-LIMIT-USD
               PERFORM 3490-ACCUM-CURRENCY
               PERFORM 3500-SET-CURRENCY
               PERFORM 3510-AGE-EFF
               PERFORM 3520-SPLIT-BUYBACK
               PERFORM 3530-SELECT-PENALTY-PCT
               PERFORM 3540-SELECT-MARGIN
               PERFORM 3550-AGE-EXPIRY
               PERFORM 3560-AGE-LAST-PAY
               PERFORM 3570-ACCRUE-PENALTY-PCT
               PERFORM 3580-AGE-LAST-ACT
               PERFORM 3590-CHECK-LAST-MAINT
               PERFORM 3600-SET-CURRENCY
               PERFORM 3610-EDIT-NAME
               PERFORM 3620-FLAG-BUYBACK
               PERFORM 3630-FLAG-USD
               PERFORM 3640-SELECT-RATE
               PERFORM 3650-CHECK-CLOSE
               PERFORM 3660-LIMIT-FOREIGN
               PERFORM 3670-SET-CHANNEL
               PERFORM 3680-CHECK-OPEN
               PERFORM 3690-SET-STATUS
               PERFORM 3700-CHECK-MATURITY
               PERFORM 3710-EDIT-OPERATOR
               PERFORM 3720-ACCUM-CURRENCY
               PERFORM 3730-AGE-MATURITY
               PERFORM 3740-SELECT-PENALTY-PCT
               PERFORM 3750-EDIT-OPERATOR
               PERFORM 3760-SPLIT-FOREIGN
               PERFORM 3770-PRORATE-USD
               PERFORM 3780-FLAG-USD
               PERFORM 3790-ACCUM-CURRENCY
               PERFORM 3800-AGE-NOTICE
               PERFORM 3810-DESCRIBE-CHANNEL
               PERFORM 3820-SELECT-MARGIN
               PERFORM 3830-AGE-LAST-MAINT
               PERFORM 3840-CHECK-EFFECTIVE
               PERFORM 3850-FLAG-BUYBACK
               PERFORM 3860-ACCUM-BRANCH
               PERFORM 3870-ACCRUE-BUYBACK
               PERFORM 3880-SELECT-MARGIN
               PERFORM 3890-ACCUM-CURRENCY
               PERFORM 3900-EDIT-REJECT
               PERFORM 3910-FLAG-FOREIGN
               PERFORM 3920-PRORATE-FOREIGN
               PERFORM 3930-PRORATE-MARGIN
               PERFORM 3940-EDIT-ADDR
               PERFORM 3950-DESCRIBE-SOURCE
               PERFORM 3960-ACCRUE-SHIPPING
               PERFORM 3970-DESCRIBE-CURRENCY
               PERFORM 3980-SELECT-MARGIN
               PERFORM 3990-PRORATE-ITEM
               PERFORM 4000-SPLIT-FOREIGN
               PERFORM 4010-ACCUM-CURRENCY
               PERFORM 4020-ACCRUE-RATE
               PERFORM 4030-FLAG-ITEM
               PERFORM 4040-ACCRUE-USD
               PERFORM 4050-DESCRIBE-CHANNEL
               PERFORM 4060-AGE-LAST-ACT
               PERFORM 4070-FLAG-BUYBACK
               PERFORM 4080-DESCRIBE-ORDER-TYPE
               PERFORM 4090-FLAG-USD
               PERFORM 4100-CHECK-LAST-PAY
               PERFORM 4110-SPLIT-SHIPPING
               PERFORM 4120-EDIT-REF-NO
               PERFORM 4130-ASSESS-AUTO-RENEW
               PERFORM 4140-DESCRIBE-SOURCE
               PERFORM 4150-FLAG-USD
               PERFORM 4160-CHECK-LAST-ACT
               PERFORM 4170-SELECT-RATE
               PERFORM 4180-ASSESS-OVERRIDE
               PERFORM 4190-ACCUM-CURRENCY
               PERFORM 4200-CHECK-OPEN
               PERFORM 4210-FLAG-BUYBACK
               PERFORM 4220-SELECT-MARGIN
               PERFORM 4230-PRORATE-USD
               PERFORM 4240-LIMIT-FOREIGN
               PERFORM 4250-AGE-EFFECTIVE
               PERFORM 4260-SELECT-MARGIN
               PERFORM 4270-SELECT-RATE
               PERFORM 4280-ACCUM-CURRENCY
               PERFORM 4290-DESCRIBE-STATUS
               PERFORM 4300-EDIT-REF-NO
               PERFORM 4310-DESCRIBE-TRAN
               PERFORM 7010-CALL-FNELG
               PERFORM 7000-CALL-FNTIER
               PERFORM 7040-CALL-FNSTAT
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3020-FLAG-USD.
           IF FXNT-USD-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-USD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-USD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E912 CUST ID REQUIRED
      * E293 SHIPPING AMT NEGATIVE
      * E614 SHIPPING AMT OVER 10000
      * E356 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-AMT < 0
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-AMT > 10000.00
                   MOVE 'E614' TO WS-REJECT-CODE
               WHEN FXNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPPING YTD BY ORDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNM-SHIPPING-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-SHIPPING-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNT-BUYBACK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNT-BUYBACK-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNT-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN YTD FEE ON THE BUYBACK YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3110-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-BUYBACK-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 7.00 TO FXNM-FOREIGN-YTD
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 10.00 TO FXNM-FOREIGN-YTD
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-BUYBACK-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-FOREIGN-YTD > 0 AND FXNM-FOREIGN-YTD < 2.00
               MOVE 2.00 TO FXNM-FOREIGN-YTD
           END-IF
           IF FXNM-FOREIGN-YTD > 25.00
               MOVE 25.00 TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E074 CUST ID REQUIRED
      * E655 USD YTD NEGATIVE
      * E419 USD YTD OVER 5000
      * E717 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD < 0
                   MOVE 'E655' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD > 5000.00
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3140-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-AMT < 0.00
               MOVE 0.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-AMT > 500.00
               MOVE 500.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNM-BUYBACK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3160-FLAG-SHIPPING.
           IF FXNM-SHIPPING-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-SHIPPING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3180-FLAG-FOREIGN.
           IF FXNM-FOREIGN-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-FOREIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN OPEN DATE IS PAST
      * FS -> HL WHEN OPEN DATE IS PAST
      * HL -> HS WHEN OPEN DATE IS PAST
      * HS -> NW WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN LAST ACT DATE IS PAST
      * FS -> HL WHEN LAST ACT DATE IS PAST
      * HL -> HS WHEN LAST ACT DATE IS PAST
      * HS -> NW WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK SCHED TIER FOR THE BUYBACK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-SCHED-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-BUYBACK-YTD NOT < WS-BUYBACK-SCHED-LOW(WS-TX)
                   MOVE WS-BUYBACK-SCHED-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-MARGIN.
           IF FXNM-MARGIN-AMT < 0.00
               MOVE 0.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON FOREIGN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNT-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E872 OPERATOR REQUIRED
      * E827 BUYBACK YTD NEGATIVE
      * E331 BUYBACK YTD OVER 5000
      * E388 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD < 0
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD > 5000.00
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD YTD.
      *----------------------------------------------------------------*
       3270-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-YTD ROUNDED =
                   FXNM-SHIPPING-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-USD-YTD
           END-IF
           ADD FXNM-USD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNM-USD-YTD / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-USD-YTD - (WS-WORK-AMT * 4)
           COMPUTE FXNM-MARGIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNT-FOREIGN-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3330-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-AMT < 0.00
               MOVE 0.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-AMT > 50000.00
               MOVE 50000.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3350-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 50.00
               MOVE 50.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 2500.00
               MOVE 2500.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-USD-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNT-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E574 REF NO REQUIRED
      * E546 WORK AMT 2 NEGATIVE
      * E992 WORK AMT 2 OVER 10000
      * E294 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E574' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E992' TO WS-REJECT-CODE
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
      * MARGIN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3400-FLAG-MARGIN.
           IF FXNM-MARGIN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN LAST PAY DATE IS PAST
      * KZV -> PHV WHEN LAST PAY DATE IS PAST
      * PHV -> PZZ WHEN LAST PAY DATE IS PAST
      * PZZ -> WMC WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNM-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN NOTICE DATE IS PAST
      * BB -> SH WHEN NOTICE DATE IS PAST
      * SH -> CN WHEN NOTICE DATE IS PAST
      * CN -> OR WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN OPEN DATE IS PAST
      * KZV -> PHV WHEN OPEN DATE IS PAST
      * PHV -> PZZ WHEN OPEN DATE IS PAST
      * PZZ -> WMC WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARGIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       3460-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-BUYBACK-AMT ROUNDED =
                   FXNM-MARGIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-MARGIN-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-MARGIN-AMT
           END-IF
           ADD FXNM-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-USD.
           IF FXNM-USD-AMT < 100.00
               MOVE 100.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 50000.00
               MOVE 50000.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN EFFECTIVE DATE IS PAST
      * E -> G WHEN EFFECTIVE DATE IS PAST
      * G -> P WHEN EFFECTIVE DATE IS PAST
      * P -> W WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNM-BUYBACK-YTD / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-BUYBACK-YTD - (WS-WORK-AMT * 2)
           COMPUTE FXNM-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-SHIPPING-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN GRID TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-GRID-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNT-MARGIN-AMT NOT < WS-MARGIN-GRID-LOW(WS-TX)
                   MOVE WS-MARGIN-GRID-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-EXPIRY.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNM-USD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN EFF DATE IS PAST
      * E -> G WHEN EFF DATE IS PAST
      * G -> P WHEN EFF DATE IS PAST
      * P -> W WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E036 NAME REQUIRED
      * E006 USD YTD NEGATIVE
      * E245 USD YTD OVER 1000000
      * E496 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E036' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD < 0
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD > 1000000.00
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN FXNM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3620-FLAG-BUYBACK.
           IF FXNM-BUYBACK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * USD YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3630-FLAG-USD.
           IF FXNM-USD-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-USD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN GRID TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-GRID-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNT-USD-AMT NOT < WS-MARGIN-GRID-LOW(WS-TX)
                   MOVE WS-MARGIN-GRID-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3660-LIMIT-FOREIGN.
           IF FXNT-FOREIGN-AMT < 0.00
               MOVE 0.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-FOREIGN-AMT > 5000.00
               MOVE 5000.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3670-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
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
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN LAST MAINT DATE IS PAST
      * ESS -> XDE WHEN LAST MAINT DATE IS PAST
      * XDE -> XFD WHEN LAST MAINT DATE IS PAST
      * XFD -> CXH WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E164 OPERATOR REQUIRED
      * E416 BUYBACK AMT NEGATIVE
      * E927 BUYBACK AMT OVER 250000
      * E541 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT < 0
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-AMT > 250000.00
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E541' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN GRID TIER FOR THE SHIPPING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-GRID-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNM-SHIPPING-YTD NOT < WS-MARGIN-GRID-LOW(WS-TX)
                   MOVE WS-MARGIN-GRID-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E893 OPERATOR REQUIRED
      * E900 SHIPPING YTD NEGATIVE
      * E139 SHIPPING YTD OVER 50000
      * E828 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD < 0
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD > 50000.00
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN FXNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-MARGIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3770-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   FXNM-USD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * USD AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3780-FLAG-USD.
           IF FXNT-USD-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNM-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING TIER TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-TIER-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF FXNT-USD-AMT NOT < WS-SHIPPING-TIER-LOW(WS-TX)
                   MOVE WS-SHIPPING-TIER-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3850-FLAG-BUYBACK.
           IF FXNM-BUYBACK-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-BRANCH-CD
                   ADD FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK SCHED TIER FOR THE FOREIGN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-SCHED-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-FOREIGN-YTD NOT < WS-BUYBACK-SCHED-LOW(WS-TX)
                   MOVE WS-BUYBACK-SCHED-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E191 REJECT CD REQUIRED
      * E139 MARGIN AMT NEGATIVE
      * E794 MARGIN AMT OVER 25000
      * E763 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT < 0
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT > 25000.00
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN FXNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3910-FLAG-FOREIGN.
           IF FXNT-FOREIGN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN YTD.
      *----------------------------------------------------------------*
       3930-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-MARGIN-YTD ROUNDED =
                   FXNM-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-MARGIN-YTD
           END-IF
           ADD FXNM-MARGIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E610 ADDR REQUIRED
      * E395 USD AMT NEGATIVE
      * E305 USD AMT OVER 5000
      * E934 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-ADDR = SPACES
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN FXNT-USD-AMT < 0
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN FXNT-USD-AMT > 5000.00
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN FXNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-AMT * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNM-USD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-MARGIN-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3990-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNT-FOREIGN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNT-FOREIGN-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNT-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNT-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4030-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-USD.
           IF FXNT-USD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-USD-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-SHIPPING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4070-FLAG-BUYBACK.
           IF FXNM-BUYBACK-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4090-FLAG-USD.
           IF FXNM-USD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-LAST-PAY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FXNM-SHIPPING-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-SHIPPING-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNT-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E393 REF NO REQUIRED
      * E701 FOREIGN AMT NEGATIVE
      * E953 FOREIGN AMT OVER 1000000
      * E328 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT < 0
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT > 1000000.00
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN FXNM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE FOREIGN AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4130-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 12.00 TO FXNM-FOREIGN-AMT
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 7.00 TO FXNM-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-FOREIGN-AMT > 0 AND FXNM-FOREIGN-AMT < 2.00
               MOVE 2.00 TO FXNM-FOREIGN-AMT
           END-IF
           IF FXNM-FOREIGN-AMT > 35.00
               MOVE 35.00 TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4150-FLAG-USD.
           IF FXNT-USD-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-TIER-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-SHIPPING-TIER-LOW(WS-TX)
                   MOVE WS-SHIPPING-TIER-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4180-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = WS-WORK-AMT-2 *
                       FXNM-MARGIN / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 15.00 TO FXNT-BUYBACK-AMT
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 10.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = WS-WORK-AMT-2 *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 25.00
               MOVE 25.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4210-FLAG-BUYBACK.
           IF FXNT-BUYBACK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK SCHED TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-SCHED-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-BUYBACK-AMT NOT < WS-BUYBACK-SCHED-LOW(WS-TX)
                   MOVE WS-BUYBACK-SCHED-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       4230-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-AMT ROUNDED =
                   FXNM-USD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4240-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-AMT < 100.00
               MOVE 100.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-AMT > 500.00
               MOVE 500.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK SCHED TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-SCHED-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNT-FOREIGN-AMT NOT < WS-BUYBACK-SCHED-LOW(WS-TX)
                   MOVE WS-BUYBACK-SCHED-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNT-MARGIN-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNT-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E496 REF NO REQUIRED
      * E674 WORK AMT NEGATIVE
      * E965 WORK AMT OVER 50000
      * E834 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN FXNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4310-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

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
                   MOVE FNELG-FOREIGN-AMT
                     TO FXNM-FOREIGN-AMT
                   MOVE FNELG-SHIPPING-AMT
                     TO FXNM-SHIPPING-AMT
                   MOVE FNELG-MARGIN-AMT
                     TO FXNM-MARGIN-AMT
                   MOVE FNELG-BUYBACK-AMT
                     TO FXNM-BUYBACK-AMT
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

      *----------------------------------------------------------------*
      * PASS THE FXN FIELDS TO FNTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-FNTIER.
           INITIALIZE FNTIER-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNTIER-FOREIGN-AMT
           MOVE FXNM-USD-AMT
             TO FNTIER-USD-AMT
           MOVE FXNM-SHIPPING-AMT
             TO FNTIER-SHIPPING-AMT
           MOVE FXNM-BUYBACK-AMT
             TO FNTIER-BUYBACK-AMT
           MOVE FXNM-BUYBACK-YTD
             TO FNTIER-BUYBACK-YTD
           MOVE FXNM-FOREIGN-YTD
             TO FNTIER-FOREIGN-YTD
           MOVE FXNM-CLOSE-DATE
             TO FNTIER-CLOSE-DATE
           MOVE FXNM-LAST-MAINT-DATE
             TO FNTIER-LAST-MAINT-DATE
           MOVE FXNM-OPEN-DATE
             TO FNTIER-OPEN-DATE
           MOVE FXNM-EFFECTIVE-DATE
             TO FNTIER-EFFECTIVE-DATE
           MOVE FXNM-STATUS-CD
             TO FNTIER-STATUS-CD
           MOVE FXNM-SOURCE-CD
             TO FNTIER-SOURCE-CD
           MOVE FXNM-TYPE-CD
             TO FNTIER-TYPE-CD
           MOVE FXNM-BRANCH-CD
             TO FNTIER-BRANCH-CD
           MOVE FXNM-AUTO-RENEW-FLAG
             TO FNTIER-AUTO-RENEW-FLAG
           MOVE FXNM-NOTICE-SENT-FLAG
             TO FNTIER-NOTICE-SENT-FLAG
           MOVE FXNM-PREFERRED-FLAG
             TO FNTIER-PREFERRED-FLAG
           MOVE FXNM-PAYMENT-CNT
             TO FNTIER-PAYMENT-CNT
           MOVE FXNM-EXCEPT-CNT
             TO FNTIER-EXCEPT-CNT
           MOVE FXNM-MARGIN
             TO FNTIER-MARGIN
           MOVE FXNM-RATE
             TO FNTIER-RATE
           MOVE FXNM-DESC
             TO FNTIER-DESC
           MOVE FXNM-REF-NO
             TO FNTIER-REF-NO
           CALL 'FNTIER' USING FNTIER-PARMS
           EVALUATE FNTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNTIER-USD-AMT
                     TO FXNM-USD-AMT
                   MOVE FNTIER-SHIPPING-AMT
                     TO FXNM-SHIPPING-AMT
                   MOVE FNTIER-BUYBACK-YTD
                     TO FXNM-BUYBACK-YTD
                   MOVE FNTIER-FOREIGN-YTD
                     TO FXNM-FOREIGN-YTD
                   MOVE FNTIER-NOTICE-SENT-FLAG
                     TO FXNM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-FNTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE FXN FIELDS TO FNSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-FNSTAT.
           INITIALIZE FNSTAT-PARMS
           MOVE FXNM-BUYBACK-AMT
             TO FNSTAT-BUYBACK-AMT
           MOVE FXNM-USD-YTD
             TO FNSTAT-USD-YTD
           MOVE FXNM-MARGIN-AMT
             TO FNSTAT-MARGIN-AMT
           MOVE FXNM-SHIPPING-YTD
             TO FNSTAT-SHIPPING-YTD
           MOVE FXNM-USD-AMT
             TO FNSTAT-USD-AMT
           MOVE FXNM-FOREIGN-AMT
             TO FNSTAT-FOREIGN-AMT
           MOVE FXNM-EXPIRY-DATE
             TO FNSTAT-EXPIRY-DATE
           MOVE FXNM-OPEN-DATE
             TO FNSTAT-OPEN-DATE
           MOVE FXNM-MATURITY-DATE
             TO FNSTAT-MATURITY-DATE
           MOVE FXNM-LAST-ACT-DATE
             TO FNSTAT-LAST-ACT-DATE
           MOVE FXNM-SOURCE-CD
             TO FNSTAT-SOURCE-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNSTAT-ORDER-TYPE-CD
           MOVE FXNM-TYPE-CD
             TO FNSTAT-TYPE-CD
           MOVE FXNM-STATUS-CD
             TO FNSTAT-STATUS-CD
           MOVE FXNM-REVIEW-FLAG
             TO FNSTAT-REVIEW-FLAG
           MOVE FXNM-OVERRIDE-FLAG
             TO FNSTAT-OVERRIDE-FLAG
           MOVE FXNM-PREFERRED-FLAG
             TO FNSTAT-PREFERRED-FLAG
           MOVE FXNM-PAYMENT-CNT
             TO FNSTAT-PAYMENT-CNT
           MOVE FXNM-NOTICE-CNT
             TO FNSTAT-NOTICE-CNT
           MOVE FXNM-RATE
             TO FNSTAT-RATE
           MOVE FXNM-PENALTY-PCT
             TO FNSTAT-PENALTY-PCT
           MOVE FXNM-ADDR
             TO FNSTAT-ADDR
           MOVE FXNM-CONTACT
             TO FNSTAT-CONTACT
           CALL 'FNSTAT' USING FNSTAT-PARMS
           EVALUATE FNSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNSTAT-MARGIN-AMT
                     TO FXNM-MARGIN-AMT
                   MOVE FNSTAT-SHIPPING-YTD
                     TO FXNM-SHIPPING-YTD
                   MOVE FNSTAT-USD-AMT
                     TO FXNM-USD-AMT
                   MOVE FNSTAT-REVIEW-FLAG
                     TO FXNM-REVIEW-FLAG
                   MOVE FNSTAT-PREFERRED-FLAG
                     TO FXNM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-FNSTAT' TO ABND-PARAGRAPH
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
           DISPLAY 'FNNPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'FNNPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
