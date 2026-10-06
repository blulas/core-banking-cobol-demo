      *================================================================*
      * PROGRAM  : FNNXMIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : FOREIGN BANKNOTES - OUTBOUND TRANSMISSION. READS THE*
      *            FXN TRANSACTIONS, UPDATES THE FXN MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. FOREIGN CURRENCY       *
      *            BANKNOTE ORDERS, BUYBACK AND MARGIN                 *
      * INPUT    : FXNMST (KSDS), FXNTRN, BANKCTL                      *
      * OUTPUT   : FXNOUT, FNNXMRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, FNTIER, FNELG           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-22 RJT  ORIGINAL                                       *
      * 1996-01-03 CRP  MARGIN ROUNDING - HALF UP REQ9380              *
      * 1996-10-17 DPS  USD LIMIT REVISED PRB7834                      *
      * 1997-06-16 GAF  NEW MARGIN REPORT COLUMNS REQ7871              *
      * 1998-05-22 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-08-18 EWS  BUYBACK LIMIT REVISED CHG2954                  *
      * 2000-12-10 PQR  AUDIT FINDING - MARGIN CONTROL TOTALS CHG5007  *
      * 2001-02-28 RJT  USD RERUN PROTECTION CHG8033                   *
      * 2002-02-27 EWS  USD LIMIT REVISED PRB7921                      *
      * 2003-07-28 BHS  SHIPPING CUTOFF MOVED PRB1228                  *
      * 2003-02-15 EWS  SHIPPING RERUN PROTECTION PRB5180              *
      * 2005-12-02 LMB  BUYBACK RERUN PROTECTION PRB3092               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNNXMIT.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "FNNXMRPT"
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
                                           VALUE 20.000000.
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
      * WS-MARGIN-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MARGIN-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000745000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000773000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000701000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000478000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000127000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006150000000470500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007150000000044000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012150000000397000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014650000000329500'.
       01  WS-MARGIN-SCHED-TBL REDEFINES WS-MARGIN-SCHED-VALUES.
           05  WS-MARGIN-SCHED-ROW OCCURS 9 TIMES.
               10  WS-MARGIN-SCHED-LOW     PIC 9(09)V99.
               10  WS-MARGIN-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUYBACK-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000441000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000411000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000130500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000172000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000075000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016750000000240000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016850000000712500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016900000000887000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000677500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022000000000559500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022050000000375000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022150000000227000'.
       01  WS-BUYBACK-GRID-TBL REDEFINES WS-BUYBACK-GRID-VALUES.
           05  WS-BUYBACK-GRID-ROW OCCURS 12 TIMES.
               10  WS-BUYBACK-GRID-LOW     PIC 9(09)V99.
               10  WS-BUYBACK-GRID-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  FNNXMIT  FOREIGN BANKNOTES'.
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
       COPY FNTIERP.
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
               PERFORM 3000-EDIT-MEMO
               PERFORM 3010-PRORATE-BUYBACK
               PERFORM 3020-ACCUM-ORDER-TYPE
               PERFORM 3030-PRORATE-MARGIN
               PERFORM 3040-CHECK-LAST-ACT
               PERFORM 3050-ASSESS-NOTICE-SENT
               PERFORM 3060-SET-STATUS
               PERFORM 3070-COUNT-EXCEPT
               PERFORM 3080-LIMIT-USD
               PERFORM 3090-DESCRIBE-STATUS
               PERFORM 3100-AGE-LAST-ACT
               PERFORM 3110-ACCUM-CHANNEL
               PERFORM 3120-ASSESS-REVIEW
               PERFORM 3130-CHECK-LAST-ACT
               PERFORM 3140-ASSESS-OVERRIDE
               PERFORM 3150-SET-STATUS
               PERFORM 3160-CHECK-LAST-ACT
               PERFORM 3170-SPLIT-MARGIN
               PERFORM 3180-ACCRUE-BUYBACK
               PERFORM 3190-SELECT-RATE
               PERFORM 3200-SPLIT-MARGIN
               PERFORM 3210-SELECT-PENALTY-PCT
               PERFORM 3220-AGE-OPEN
               PERFORM 3230-AGE-EFF
               PERFORM 3240-FLAG-MARGIN
               PERFORM 3250-SET-CURRENCY
               PERFORM 3260-ACCUM-SOURCE
               PERFORM 3270-ACCRUE-RATE
               PERFORM 3280-AGE-CLOSE
               PERFORM 3290-SELECT-MARGIN
               PERFORM 3300-DESCRIBE-SOURCE
               PERFORM 3310-EDIT-DESC
               PERFORM 3320-AGE-LAST-MAINT
               PERFORM 3330-SELECT-MARGIN
               PERFORM 3340-SPLIT-FOREIGN
               PERFORM 3350-SET-STATUS
               PERFORM 3360-SET-SOURCE
               PERFORM 3370-DESCRIBE-CHANNEL
               PERFORM 3380-COUNT-NOTICE
               PERFORM 3390-PRORATE-BUYBACK
               PERFORM 3400-ACCUM-STATUS
               PERFORM 3410-COUNT-EXCEPT
               PERFORM 3420-ACCUM-SOURCE
               PERFORM 3430-SPLIT-SHIPPING
               PERFORM 3440-SPLIT-FOREIGN
               PERFORM 3450-CHECK-POST
               PERFORM 3460-FLAG-ITEM
               PERFORM 3470-ASSESS-AUTO-RENEW
               PERFORM 3480-ACCUM-TRAN
               PERFORM 3490-COUNT-NOTICE
               PERFORM 3500-SET-TRAN
               PERFORM 3510-COUNT-EXCEPT
               PERFORM 3520-SET-CHANNEL
               PERFORM 3530-PRORATE-SHIPPING
               PERFORM 3540-DESCRIBE-ORDER-TYPE
               PERFORM 3550-ACCRUE-SHIPPING
               PERFORM 3560-SPLIT-USD
               PERFORM 3570-ACCRUE-FOREIGN
               PERFORM 3580-ASSESS-NOTICE-SENT
               PERFORM 3590-SELECT-RATE
               PERFORM 3600-COUNT-EXCEPT
               PERFORM 3610-ASSESS-REVIEW
               PERFORM 3620-PRORATE-FOREIGN
               PERFORM 3630-COUNT-PAYMENT
               PERFORM 3640-DESCRIBE-BRANCH
               PERFORM 3650-SELECT-RATE
               PERFORM 3660-ACCUM-BRANCH
               PERFORM 3670-DESCRIBE-STATUS
               PERFORM 3680-ASSESS-REVERSAL
               PERFORM 3690-DESCRIBE-TYPE
               PERFORM 3700-FLAG-BUYBACK
               PERFORM 3710-COUNT-NOTICE
               PERFORM 3720-SET-CURRENCY
               PERFORM 3730-PRORATE-USD
               PERFORM 3740-SET-STATUS
               PERFORM 3750-ACCRUE-RATE
               PERFORM 3760-ACCUM-SOURCE
               PERFORM 3770-LIMIT-MARGIN
               PERFORM 3780-ASSESS-REVIEW
               PERFORM 3790-SET-CHANNEL
               PERFORM 3800-ASSESS-OVERRIDE
               PERFORM 3810-DESCRIBE-TRAN
               PERFORM 3820-AGE-POST
               PERFORM 3830-SET-SOURCE
               PERFORM 3840-COUNT-SEQ
               PERFORM 3850-CHECK-MATURITY
               PERFORM 3860-SET-BRANCH
               PERFORM 3870-COUNT-EXCEPT
               PERFORM 3880-CHECK-LAST-PAY
               PERFORM 3890-CHECK-LAST-ACT
               PERFORM 3900-AGE-EFFECTIVE
               PERFORM 3910-ASSESS-NOTICE-SENT
               PERFORM 3920-LIMIT-BUYBACK
               PERFORM 3930-ASSESS-REVIEW
               PERFORM 3940-ACCRUE-BUYBACK
               PERFORM 3950-SELECT-RATE
               PERFORM 3960-LIMIT-FOREIGN
               PERFORM 3970-PRORATE-MARGIN
               PERFORM 3980-COUNT-CYCLE
               PERFORM 3990-DESCRIBE-CHANNEL
               PERFORM 4000-CHECK-NOTICE
               PERFORM 4010-AGE-LAST-PAY
               PERFORM 4020-SPLIT-MARGIN
               PERFORM 4030-FLAG-FOREIGN
               PERFORM 4040-LIMIT-USD
               PERFORM 4050-LIMIT-SHIPPING
               PERFORM 4060-COUNT-EXCEPT
               PERFORM 4070-ASSESS-REVERSAL
               PERFORM 4080-SPLIT-USD
               PERFORM 4090-CHECK-EXPIRY
               PERFORM 4100-ASSESS-EXEMPT
               PERFORM 4110-ASSESS-REVERSAL
               PERFORM 4120-EDIT-DESC
               PERFORM 4130-LIMIT-USD
               PERFORM 4140-FLAG-BUYBACK
               PERFORM 4150-FLAG-ITEM
               PERFORM 4160-FLAG-ITEM
               PERFORM 4170-SELECT-MARGIN
               PERFORM 4180-SET-ORDER-TYPE
               PERFORM 4190-DESCRIBE-TYPE
               PERFORM 4200-CHECK-EFFECTIVE
               PERFORM 4210-EDIT-CUST-ID
               PERFORM 4220-AGE-LAST-PAY
               PERFORM 4230-LIMIT-SHIPPING
               PERFORM 4240-CHECK-EXPIRY
               PERFORM 4250-SELECT-MARGIN
               PERFORM 4260-EDIT-REJECT
               PERFORM 4270-EDIT-REF-NO
               PERFORM 4280-LIMIT-USD
               PERFORM 4290-EDIT-NAME
               PERFORM 4300-EDIT-CONTACT
               PERFORM 4310-EDIT-NAME
               PERFORM 4320-ASSESS-REVIEW
               PERFORM 4330-PRORATE-SHIPPING
               PERFORM 4340-PRORATE-BUYBACK
               PERFORM 4350-COUNT-NOTICE
               PERFORM 4360-COUNT-EXCEPT
               PERFORM 4370-LIMIT-ITEM
               PERFORM 4380-SPLIT-MARGIN
               PERFORM 4390-PRORATE-ITEM
               PERFORM 4400-LIMIT-MARGIN
               PERFORM 4410-DESCRIBE-BRANCH
               PERFORM 4420-COUNT-NOTICE
               PERFORM 4430-ACCUM-TYPE
               PERFORM 4440-CHECK-LAST-PAY
               PERFORM 4450-ACCRUE-MARGIN
               PERFORM 4460-EDIT-DESC
               PERFORM 4470-PRORATE-MARGIN
               PERFORM 4480-ASSESS-EXEMPT
               PERFORM 4490-ACCUM-ORDER-TYPE
               PERFORM 4500-AGE-LAST-MAINT
               PERFORM 4510-COUNT-CYCLE
               PERFORM 4520-ASSESS-NOTICE-SENT
               PERFORM 4530-EDIT-REJECT
               PERFORM 4540-AGE-EXPIRY
               PERFORM 4550-DESCRIBE-CHANNEL
               PERFORM 4560-EDIT-OPERATOR
               PERFORM 4570-SELECT-RATE
               PERFORM 4580-ACCRUE-MARGIN
               PERFORM 4590-DESCRIBE-SOURCE
               PERFORM 4600-DESCRIBE-TYPE
               PERFORM 4610-SET-CHANNEL
               PERFORM 4620-FLAG-USD
               PERFORM 4630-DESCRIBE-SOURCE
               PERFORM 4640-ACCRUE-BUYBACK
               PERFORM 4650-FLAG-ITEM
               PERFORM 4660-COUNT-EXCEPT
               PERFORM 4670-AGE-OPEN
               PERFORM 7000-CALL-FNTIER
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E697 MEMO REQUIRED
      * E213 FOREIGN AMT NEGATIVE
      * E643 FOREIGN AMT OVER 25000
      * E247 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-MEMO = SPACES
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN FXNT-FOREIGN-AMT < 0
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN FXNT-FOREIGN-AMT > 25000.00
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN FXNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       3010-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-BUYBACK-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY ORDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3020-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       3030-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-MARGIN-AMT ROUNDED =
                   FXNT-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-LAST-ACT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE BUYBACK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3050-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 20.00 TO FXNT-BUYBACK-AMT
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 10.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 2.00
               MOVE 2.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 250.00
               MOVE 250.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN EFF DATE IS PAST
      * ESS -> XDE WHEN EFF DATE IS PAST
      * XDE -> XFD WHEN EFF DATE IS PAST
      * XFD -> CXH WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-USD.
           IF FXNT-USD-AMT < 100.00
               MOVE 100.00 TO FXNT-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-USD-AMT > 10000.00
               MOVE 10000.00 TO FXNT-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-LAST-ACT.
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
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-CHANNEL.
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
      * ASSESS THE BUYBACK FEE ON THE USD AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3120-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 5.00 TO FXNM-BUYBACK-AMT
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 5.00 TO FXNM-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-BUYBACK-AMT > 0 AND FXNM-BUYBACK-AMT < 2.00
               MOVE 2.00 TO FXNM-BUYBACK-AMT
           END-IF
           IF FXNM-BUYBACK-AMT > 150.00
               MOVE 150.00 TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-LAST-ACT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE USD YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3140-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-USD-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 3.00 TO FXNT-BUYBACK-AMT
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 7.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-USD-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 2.00
               MOVE 2.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 250.00
               MOVE 250.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN POST DATE IS PAST
      * ESS -> XDE WHEN POST DATE IS PAST
      * XDE -> XFD WHEN POST DATE IS PAST
      * XFD -> CXH WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-STATUS.
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
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-LAST-ACT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-YTD - (WS-WORK-AMT * 4)
           COMPUTE FXNT-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-BUYBACK.
           IF FXNM-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK GRID TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-GRID-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-FOREIGN-AMT NOT < WS-BUYBACK-GRID-LOW(WS-TX)
                   MOVE WS-BUYBACK-GRID-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-YTD / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-YTD - (WS-WORK-AMT * 12)
           COMPUTE FXNM-BUYBACK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN SCHED TIER FOR THE MARGIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-SCHED-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNM-MARGIN-YTD NOT < WS-MARGIN-SCHED-LOW(WS-TX)
                   MOVE WS-MARGIN-SCHED-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-OPEN.
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
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-EFF.
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
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3240-FLAG-MARGIN.
           IF FXNM-MARGIN-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN LAST PAY DATE IS PAST
      * E -> G WHEN LAST PAY DATE IS PAST
      * G -> P WHEN LAST PAY DATE IS PAST
      * P -> W WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNT-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK GRID TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-GRID-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-BUYBACK-AMT NOT < WS-BUYBACK-GRID-LOW(WS-TX)
                   MOVE WS-BUYBACK-GRID-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E893 DESC REQUIRED
      * E363 MARGIN YTD NEGATIVE
      * E549 MARGIN YTD OVER 25000
      * E467 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD < 0
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD > 25000.00
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN FXNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-MAINT.
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
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK GRID TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-GRID-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNM-BUYBACK-AMT NOT < WS-BUYBACK-GRID-LOW(WS-TX)
                   MOVE WS-BUYBACK-GRID-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-FOREIGN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN OPEN DATE IS PAST
      * ESS -> XDE WHEN OPEN DATE IS PAST
      * XDE -> XFD WHEN OPEN DATE IS PAST
      * XFD -> CXH WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN EFFECTIVE DATE IS PAST
      * FS -> HL WHEN EFFECTIVE DATE IS PAST
      * HL -> HS WHEN EFFECTIVE DATE IS PAST
      * HS -> NW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       3390-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-BUYBACK-AMT ROUNDED =
                   FXNT-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FXNM-SHIPPING-YTD / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-SHIPPING-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-YTD / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-YTD - (WS-WORK-AMT * 2)
           COMPUTE FXNM-MARGIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3460-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE FOREIGN YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3470-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-RATE / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 3.00 TO FXNM-FOREIGN-AMT
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 12.00 TO FXNM-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-FOREIGN-AMT > 0 AND FXNM-FOREIGN-AMT < 10.00
               MOVE 10.00 TO FXNM-FOREIGN-AMT
           END-IF
           IF FXNM-FOREIGN-AMT > 50.00
               MOVE 50.00 TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPPING YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNM-SHIPPING-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-SHIPPING-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN POST DATE IS PAST
      * BB -> SH WHEN POST DATE IS PAST
      * SH -> CN WHEN POST DATE IS PAST
      * CN -> OR WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       3530-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-BUYBACK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT USD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNM-USD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-USD-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNM-USD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNM-USD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN YTD FEE ON THE USD YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3580-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-USD-YTD *
                       FXNM-RATE / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 10.00 TO FXNM-FOREIGN-YTD
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 12.00 TO FXNM-FOREIGN-YTD
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-USD-YTD *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-FOREIGN-YTD > 0 AND FXNM-FOREIGN-YTD < 1.00
               MOVE 1.00 TO FXNM-FOREIGN-YTD
           END-IF
           IF FXNM-FOREIGN-YTD > 25.00
               MOVE 25.00 TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN SCHED TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNT-FOREIGN-AMT NOT < WS-MARGIN-SCHED-LOW(WS-TX)
                   MOVE WS-MARGIN-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE BUYBACK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3610-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FXNT-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 10.00 TO FXNT-FOREIGN-AMT
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 15.00 TO FXNT-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNT-FOREIGN-AMT > 0 AND FXNT-FOREIGN-AMT < 5.00
               MOVE 5.00 TO FXNT-FOREIGN-AMT
           END-IF
           IF FXNT-FOREIGN-AMT > 75.00
               MOVE 75.00 TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-AMT ROUNDED =
                   FXNT-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO FXNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN SCHED TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNT-FOREIGN-AMT NOT < WS-MARGIN-SCHED-LOW(WS-TX)
                   MOVE WS-MARGIN-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-BRANCH-CD
                   ADD FXNM-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN YTD FEE ON THE FOREIGN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3680-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 7.00 TO FXNM-FOREIGN-YTD
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 5.00 TO FXNM-FOREIGN-YTD
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-FOREIGN-YTD > 0 AND FXNM-FOREIGN-YTD < 5.00
               MOVE 5.00 TO FXNM-FOREIGN-YTD
           END-IF
           IF FXNM-FOREIGN-YTD > 75.00
               MOVE 75.00 TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3700-FLAG-BUYBACK.
           IF FXNM-BUYBACK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN NOTICE DATE IS PAST
      * E -> G WHEN NOTICE DATE IS PAST
      * G -> P WHEN NOTICE DATE IS PAST
      * P -> W WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-USD-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN EFFECTIVE DATE IS PAST
      * ESS -> XDE WHEN EFFECTIVE DATE IS PAST
      * XDE -> XFD WHEN EFFECTIVE DATE IS PAST
      * XFD -> CXH WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNM-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-MARGIN.
           IF FXNT-MARGIN-AMT < 25.00
               MOVE 25.00 TO FXNT-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FXNT-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE MARGIN AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3780-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 7.00 TO FXNM-SHIPPING-AMT
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 7.00 TO FXNM-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-SHIPPING-AMT > 0 AND FXNM-SHIPPING-AMT < 1.00
               MOVE 1.00 TO FXNM-SHIPPING-AMT
           END-IF
           IF FXNM-SHIPPING-AMT > 75.00
               MOVE 75.00 TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-CHANNEL.
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
      * ASSESS THE BUYBACK FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3800-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = WS-WORK-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 7.00 TO FXNT-BUYBACK-AMT
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 15.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = WS-WORK-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 50.00
               MOVE 50.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-POST.
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
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN MATURITY DATE IS PAST
      * FS -> HL WHEN MATURITY DATE IS PAST
      * HL -> HS WHEN MATURITY DATE IS PAST
      * HS -> NW WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-SEQ.
           IF FXNT-SEQ < 9999999
               ADD 1 TO FXNT-SEQ
           END-IF
           IF FXNT-SEQ NOT < 12
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-MATURITY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN LAST MAINT DATE IS PAST
      * BBP -> ENL WHEN LAST MAINT DATE IS PAST
      * ENL -> EZB WHEN LAST MAINT DATE IS PAST
      * EZB -> KHV WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-LAST-PAY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-LAST-ACT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-EFFECTIVE.
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
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK YTD FEE ON THE FOREIGN AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3910-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-BUYBACK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-BUYBACK-YTD ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 7.00 TO FXNM-BUYBACK-YTD
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 15.00 TO FXNM-BUYBACK-YTD
               WHEN OTHER
                   COMPUTE FXNM-BUYBACK-YTD ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-BUYBACK-YTD > 0 AND FXNM-BUYBACK-YTD < 1.00
               MOVE 1.00 TO FXNM-BUYBACK-YTD
           END-IF
           IF FXNM-BUYBACK-YTD > 250.00
               MOVE 250.00 TO FXNM-BUYBACK-YTD
           END-IF
           ADD FXNM-BUYBACK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 100.00
               MOVE 100.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 50000.00
               MOVE 50000.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE USD YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3930-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNM-USD-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 10.00 TO FXNM-FOREIGN-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 10.00 TO FXNM-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-AMT ROUNDED = FXNM-USD-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-FOREIGN-AMT > 0 AND FXNM-FOREIGN-AMT < 1.00
               MOVE 1.00 TO FXNM-FOREIGN-AMT
           END-IF
           IF FXNM-FOREIGN-AMT > 100.00
               MOVE 100.00 TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO FXNM-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN SCHED TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNM-SHIPPING-AMT NOT < WS-MARGIN-SCHED-LOW(WS-TX)
                   MOVE WS-MARGIN-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-AMT < 10.00
               MOVE 10.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-AMT > 1000.00
               MOVE 1000.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-MARGIN-AMT ROUNDED =
                   FXNM-SHIPPING-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-MARGIN-AMT
           END-IF
           ADD FXNM-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-NOTICE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-LAST-PAY.
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
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MARGIN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-YTD - (WS-WORK-AMT * 4)
           COMPUTE FXNM-FOREIGN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4030-FLAG-FOREIGN.
           IF FXNT-FOREIGN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNT-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4040-LIMIT-USD.
           IF FXNT-USD-AMT < 100.00
               MOVE 100.00 TO FXNT-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-USD-AMT > 1000.00
               MOVE 1000.00 TO FXNT-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-SHIPPING.
           IF FXNM-SHIPPING-AMT < 25.00
               MOVE 25.00 TO FXNM-SHIPPING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-SHIPPING-AMT > 1000.00
               MOVE 1000.00 TO FXNM-SHIPPING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE USD AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4070-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNT-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 15.00 TO FXNT-USD-AMT
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 12.00 TO FXNT-USD-AMT
               WHEN OTHER
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNT-USD-AMT > 0 AND FXNT-USD-AMT < 2.00
               MOVE 2.00 TO FXNT-USD-AMT
           END-IF
           IF FXNT-USD-AMT > 250.00
               MOVE 250.00 TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNM-USD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-USD-YTD - (WS-WORK-AMT * 2)
           COMPUTE FXNM-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-EXPIRY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE MARGIN YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4100-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-MARGIN-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-MARGIN-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SHIPPING YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4110-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-SHIPPING-YTD *
                       FXNM-RATE / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-SHIPPING-YTD *
                       FXNM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E469 DESC REQUIRED
      * E140 MARGIN YTD NEGATIVE
      * E804 MARGIN YTD OVER 99999
      * E900 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD < 0
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD > 99999.00
                   MOVE 'E804' TO WS-REJECT-CODE
               WHEN FXNM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-USD.
           IF FXNM-USD-AMT < 100.00
               MOVE 100.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 10000.00
               MOVE 10000.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4140-FLAG-BUYBACK.
           IF FXNM-BUYBACK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4150-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4160-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN SCHED TIER FOR THE USD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-SCHED-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNM-USD-YTD NOT < WS-MARGIN-SCHED-LOW(WS-TX)
                   MOVE WS-MARGIN-SCHED-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN EFF DATE IS PAST
      * KZV -> PHV WHEN EFF DATE IS PAST
      * PHV -> PZZ WHEN EFF DATE IS PAST
      * PZZ -> WMC WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4180-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E745 CUST ID REQUIRED
      * E496 MARGIN YTD NEGATIVE
      * E238 MARGIN YTD OVER 25000
      * E491 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD < 0
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD > 25000.00
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E491' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-LAST-PAY.
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
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPPING YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-SHIPPING.
           IF FXNM-SHIPPING-YTD < 25.00
               MOVE 25.00 TO FXNM-SHIPPING-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-SHIPPING-YTD > 5000.00
               MOVE 5000.00 TO FXNM-SHIPPING-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-EXPIRY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK GRID TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-GRID-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF FXNT-MARGIN-AMT NOT < WS-BUYBACK-GRID-LOW(WS-TX)
                   MOVE WS-BUYBACK-GRID-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E967 REJECT CD REQUIRED
      * E147 USD AMT NEGATIVE
      * E806 USD AMT OVER 250000
      * E216 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4260-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E967' TO WS-REJECT-CODE
               WHEN FXNM-USD-AMT < 0
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN FXNM-USD-AMT > 250000.00
                   MOVE 'E806' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E216' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E100 REF NO REQUIRED
      * E775 USD YTD NEGATIVE
      * E926 USD YTD OVER 10000
      * E960 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD < 0
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD > 10000.00
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-USD.
           IF FXNM-USD-AMT < 10.00
               MOVE 10.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 10000.00
               MOVE 10000.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E491 NAME REQUIRED
      * E989 FOREIGN AMT NEGATIVE
      * E195 FOREIGN AMT OVER 250000
      * E329 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E491' TO WS-REJECT-CODE
               WHEN FXNT-FOREIGN-AMT < 0
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN FXNT-FOREIGN-AMT > 250000.00
                   MOVE 'E195' TO WS-REJECT-CODE
               WHEN FXNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E329' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E287 CONTACT REQUIRED
      * E026 SHIPPING YTD NEGATIVE
      * E175 SHIPPING YTD OVER 99999
      * E079 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CONTACT = SPACES
                   MOVE 'E287' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD < 0
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD > 99999.00
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN FXNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E079' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E414 NAME REQUIRED
      * E289 MARGIN AMT NEGATIVE
      * E585 MARGIN AMT OVER 99999
      * E605 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E414' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT < 0
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT > 99999.00
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE BUYBACK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4320-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       4330-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       4340-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-BUYBACK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO FXNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4360-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO FXNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4370-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4390-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4400-LIMIT-MARGIN.
           IF FXNM-MARGIN-AMT < 10.00
               MOVE 10.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-AMT > 500.00
               MOVE 500.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4410-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4420-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4430-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-TYPE-CD
                   ADD FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4440-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4450-ACCRUE-MARGIN.
           IF FXNT-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-MARGIN-AMT * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNM-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E297 DESC REQUIRED
      * E211 WORK AMT 2 NEGATIVE
      * E810 WORK AMT 2 OVER 5000
      * E934 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4460-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E810' TO WS-REJECT-CODE
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
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4470-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   FXNT-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4480-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = WS-WORK-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 5.00 TO FXNM-BUYBACK-AMT
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 20.00 TO FXNM-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = WS-WORK-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-BUYBACK-AMT > 0 AND FXNM-BUYBACK-AMT < 2.00
               MOVE 2.00 TO FXNM-BUYBACK-AMT
           END-IF
           IF FXNM-BUYBACK-AMT > 50.00
               MOVE 50.00 TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY ORDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4490-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-LAST-MAINT.
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
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4510-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO FXNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FOREIGN AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4520-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E598 REJECT CD REQUIRED
      * E836 USD YTD NEGATIVE
      * E550 USD YTD OVER 50000
      * E868 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4530-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD < 0
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD > 50000.00
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN FXNM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4540-AGE-EXPIRY.
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
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4550-DESCRIBE-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E602 OPERATOR REQUIRED
      * E193 FOREIGN YTD NEGATIVE
      * E013 FOREIGN YTD OVER 50000
      * E505 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4560-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD < 0
                   MOVE 'E193' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD > 50000.00
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN SCHED TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4570-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF FXNM-BUYBACK-AMT NOT < WS-MARGIN-SCHED-LOW(WS-TX)
                   MOVE WS-MARGIN-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4580-ACCRUE-MARGIN.
           IF FXNM-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-MARGIN-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4590-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4600-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4610-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4620-FLAG-USD.
           IF FXNT-USD-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4630-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4640-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4650-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4660-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4670-AGE-OPEN.
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
                   MOVE FNTIER-BUYBACK-AMT
                     TO FXNM-BUYBACK-AMT
                   MOVE FNTIER-FOREIGN-YTD
                     TO FXNM-FOREIGN-YTD
                   MOVE FNTIER-AUTO-RENEW-FLAG
                     TO FXNM-AUTO-RENEW-FLAG
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
                   MOVE FNELG-SHIPPING-AMT
                     TO FXNM-SHIPPING-AMT
                   MOVE FNELG-MARGIN-YTD
                     TO FXNM-MARGIN-YTD
                   MOVE FNELG-MARGIN-AMT
                     TO FXNM-MARGIN-AMT
                   MOVE FNELG-OVERRIDE-FLAG
                     TO FXNM-OVERRIDE-FLAG
                   MOVE FNELG-PREFERRED-FLAG
                     TO FXNM-PREFERRED-FLAG
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
           DISPLAY 'FNNXMIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'FNNXMIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
