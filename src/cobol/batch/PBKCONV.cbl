      *================================================================*
      * PROGRAM  : PBKCONV                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PASSBOOK PRINTING - FILE CONVERSION. READS THE PBK  *
      *            TRANSACTIONS, UPDATES THE PBK MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PASSBOOK SAVINGS UPDATE PRINTING*
      *            AND UNPOSTED ITEM CONTROL                           *
      * INPUT    : PBKMST (KSDS), PBKTRN, BANKCTL                      *
      * OUTPUT   : PBKOUT, PBKCORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PBSTAT, PBDTE           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-07-25 MKL  ORIGINAL                                       *
      * 1996-01-03 SKN  PRINTED EXCEPTIONS TO OPS QUEUE INC5025        *
      * 1997-07-23 LMB  BALANCE NOTICE TEXT PER LEGAL CHG2899          *
      * 1999-09-16 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-17 LMB  UNPOSTED FEE SCHEDULE UPDATE INC1435           *
      * 2008-01-09 RJT  PERFORMANCE - PRINTED READ AHEAD REQ9300       *
      * 2008-01-24 GAF  PRINTED EXCEPTIONS TO OPS QUEUE PRB5272        *
      * 2010-12-05 TWH  PERFORMANCE - LINE READ AHEAD CHG6449          *
      * 2012-04-02 EWS  INTEREST LIMIT REVISED REQ1098                 *
      * 2012-09-11 RJT  INTEREST EXCEPTIONS TO OPS QUEUE REQ8539       *
      * 2013-10-23 GAF  CARRYFWD FEE SCHEDULE UPDATE INC1337           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBKCONV.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PBKMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PBKM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PBKTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PBKOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PBKCORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PBKMST.
       FD  TRAN-FILE.
       COPY PBKTRN.
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
                                           VALUE 10.000000.
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
      * WS-PRINTED-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINTED-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000214000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000360000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000855000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000604500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004050000000152000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000157000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000789000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000515000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022050000000058500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024550000000572500'.
       01  WS-PRINTED-BAND-TBL REDEFINES WS-PRINTED-BAND-VALUES.
           05  WS-PRINTED-BAND-ROW OCCURS 10 TIMES.
               10  WS-PRINTED-BAND-LOW     PIC 9(09)V99.
               10  WS-PRINTED-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINTED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINTED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000587000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000837500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000665500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000454000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002100000000285500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002150000000524000'.
       01  WS-PRINTED-SCHED-TBL REDEFINES WS-PRINTED-SCHED-VALUES.
           05  WS-PRINTED-SCHED-ROW OCCURS 6 TIMES.
               10  WS-PRINTED-SCHED-LOW    PIC 9(09)V99.
               10  WS-PRINTED-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BALANCE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BALANCE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000392500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000455500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000322500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000128500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000475000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023300000000543500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023350000000158000'.
       01  WS-BALANCE-GRID-TBL REDEFINES WS-BALANCE-GRID-VALUES.
           05  WS-BALANCE-GRID-ROW OCCURS 8 TIMES.
               10  WS-BALANCE-GRID-LOW     PIC 9(09)V99.
               10  WS-BALANCE-GRID-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PBKCONV  PASSBOOK PRINTING'.
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
       COPY PBSTATP.
       COPY PBDTEP.

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
               MOVE 'PBKMST' TO ABND-FILE-NAME
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
           MOVE PBKT-KEY TO PBKM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-FLAG-LINE
               PERFORM 3010-SPLIT-UNPOSTED
               PERFORM 3020-COUNT-ITEM
               PERFORM 3030-COUNT-ITEM
               PERFORM 3040-DESCRIBE-REGION
               PERFORM 3050-COUNT-ITEM
               PERFORM 3060-EDIT-CONTACT
               PERFORM 3070-EDIT-CONTACT
               PERFORM 3080-SELECT-MARGIN
               PERFORM 3090-EDIT-DESC
               PERFORM 3100-ACCUM-BRANCH
               PERFORM 3110-ACCRUE-FEE-PCT
               PERFORM 3120-ASSESS-EXEMPT
               PERFORM 3130-FLAG-INTEREST
               PERFORM 3140-COUNT-PAYMENT
               PERFORM 3150-PRORATE-PRINTED
               PERFORM 3160-CHECK-EFF
               PERFORM 3170-PRORATE-LINE
               PERFORM 3180-FLAG-LINE
               PERFORM 3190-PRORATE-CARRYFWD
               PERFORM 3200-COUNT-SEQ
               PERFORM 3210-AGE-LAST-MAINT
               PERFORM 3220-SELECT-PENALTY-PCT
               PERFORM 3230-CHECK-CLOSE
               PERFORM 3240-LIMIT-INTEREST
               PERFORM 3250-SET-TRAN
               PERFORM 3260-SELECT-PENALTY-PCT
               PERFORM 3270-AGE-EXPIRY
               PERFORM 3280-ASSESS-REVIEW
               PERFORM 3290-DESCRIBE-CHANNEL
               PERFORM 3300-SPLIT-INTEREST
               PERFORM 3310-EDIT-CUST-ID
               PERFORM 3320-ACCRUE-INTEREST
               PERFORM 3330-ACCUM-CHANNEL
               PERFORM 3340-DESCRIBE-CHANNEL
               PERFORM 3350-ASSESS-REVERSAL
               PERFORM 3360-PRORATE-INTEREST
               PERFORM 3370-AGE-EXPIRY
               PERFORM 3380-SET-SOURCE
               PERFORM 3390-ASSESS-SUSPEND
               PERFORM 3400-SELECT-MARGIN
               PERFORM 3410-DESCRIBE-CHANNEL
               PERFORM 3420-SPLIT-INTEREST
               PERFORM 3430-CHECK-EFF
               PERFORM 3440-EDIT-CUST-ID
               PERFORM 3450-DESCRIBE-TRAN
               PERFORM 3460-DESCRIBE-BRANCH
               PERFORM 3470-ACCUM-BOOK-TYPE
               PERFORM 3480-SPLIT-UNPOSTED
               PERFORM 3490-LIMIT-PRINTED
               PERFORM 3500-COUNT-ITEM
               PERFORM 3510-FLAG-UNPOSTED
               PERFORM 3520-DESCRIBE-BOOK-TYPE
               PERFORM 3530-ACCUM-BOOK-TYPE
               PERFORM 3540-ASSESS-EXEMPT
               PERFORM 3550-ASSESS-HOLD
               PERFORM 3560-EDIT-NAME
               PERFORM 3570-ACCUM-BOOK-TYPE
               PERFORM 3580-SELECT-MARGIN
               PERFORM 3590-ASSESS-HOLD
               PERFORM 3600-EDIT-MEMO
               PERFORM 3610-SET-BOOK-TYPE
               PERFORM 3620-PRORATE-CARRYFWD
               PERFORM 3630-PRORATE-CARRYFWD
               PERFORM 3640-ASSESS-HOLD
               PERFORM 3650-ACCRUE-BALANCE
               PERFORM 3660-PRORATE-INTEREST
               PERFORM 3670-SET-TRAN
               PERFORM 3680-AGE-CLOSE
               PERFORM 3690-DESCRIBE-REGION
               PERFORM 3700-AGE-CLOSE
               PERFORM 3710-COUNT-PAYMENT
               PERFORM 3720-COUNT-PAYMENT
               PERFORM 3730-AGE-OPEN
               PERFORM 3740-FLAG-INTEREST
               PERFORM 3750-CHECK-LAST-MAINT
               PERFORM 3760-EDIT-CUST-ID
               PERFORM 3770-FLAG-PRINTED
               PERFORM 3780-FLAG-UNPOSTED
               PERFORM 3790-ASSESS-REVERSAL
               PERFORM 3800-SET-BOOK-TYPE
               PERFORM 3810-DESCRIBE-CHANNEL
               PERFORM 3820-EDIT-ADDR
               PERFORM 3830-SET-SOURCE
               PERFORM 3840-SELECT-MARGIN
               PERFORM 3850-DESCRIBE-REGION
               PERFORM 3860-SPLIT-LINE
               PERFORM 3870-FLAG-CARRYFWD
               PERFORM 3880-SET-BRANCH
               PERFORM 3890-ASSESS-SUSPEND
               PERFORM 3900-ASSESS-AUTO-RENEW
               PERFORM 3910-DESCRIBE-BRANCH
               PERFORM 3920-SELECT-PENALTY-PCT
               PERFORM 3930-SPLIT-LINE
               PERFORM 3940-DESCRIBE-PRINTER
               PERFORM 3950-FLAG-UNPOSTED
               PERFORM 3960-DESCRIBE-REGION
               PERFORM 3970-AGE-NEXT-DUE
               PERFORM 3980-AGE-EXPIRY
               PERFORM 3990-LIMIT-CARRYFWD
               PERFORM 4000-ACCUM-REGION
               PERFORM 4010-SELECT-MARGIN
               PERFORM 7020-CALL-PBSTAT
               PERFORM 7010-CALL-PBDTE
               REWRITE PBK-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PBKMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PBK-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * LINE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3000-FLAG-LINE.
           IF PBKT-LINE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKT-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBKM-UNPOSTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-UNPOSTED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-UNPOSTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 5
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 6
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 3
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E947 CONTACT REQUIRED
      * E608 LINE AMT NEGATIVE
      * E430 LINE AMT OVER 5000
      * E448 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CONTACT = SPACES
                   MOVE 'E947' TO WS-REJECT-CODE
               WHEN PBKT-LINE-AMT < 0
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN PBKT-LINE-AMT > 5000.00
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN PBKM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E645 CONTACT REQUIRED
      * E620 PRINTED AMT NEGATIVE
      * E711 PRINTED AMT OVER 99999
      * E440 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CONTACT = SPACES
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-AMT < 0
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-AMT > 99999.00
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN PBKM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE GRID TIER FOR THE PRINTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKT-PRINTED-AMT NOT < WS-BALANCE-GRID-LOW(WS-TX)
                   MOVE WS-BALANCE-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E272 DESC REQUIRED
      * E797 BALANCE AMT NEGATIVE
      * E599 BALANCE AMT OVER 10000
      * E251 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-AMT < 0
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-AMT > 10000.00
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE LINE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3120-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKT-LINE-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-BOOK-TYPE-CD = 'CH'
                   MOVE 10.00 TO PBKM-INTEREST-YTD
               WHEN PBKM-BOOK-TYPE-CD = 'HG'
                   MOVE 12.00 TO PBKM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKT-LINE-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-YTD > 0 AND PBKM-INTEREST-YTD < 1.00
               MOVE 1.00 TO PBKM-INTEREST-YTD
           END-IF
           IF PBKM-INTEREST-YTD > 100.00
               MOVE 100.00 TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3130-FLAG-INTEREST.
           IF PBKM-INTEREST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINTED YTD.
      *----------------------------------------------------------------*
       3150-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-YTD ROUNDED =
                   PBKM-CARRYFWD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PBKT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-LINE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-LINE-AMT
           END-IF
           ADD PBKM-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LINE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3180-FLAG-LINE.
           IF PBKM-LINE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-CARRYFWD-AMT ROUNDED =
                   PBKM-CARRYFWD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKT-CARRYFWD-AMT
           END-IF
           ADD PBKT-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 3
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED BAND TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-BAND-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKM-INTEREST-YTD NOT < WS-PRINTED-BAND-LOW(WS-TX)
                   MOVE WS-PRINTED-BAND-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3240-LIMIT-INTEREST.
           IF PBKM-INTEREST-AMT < 100.00
               MOVE 100.00 TO PBKM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-INTEREST-AMT > 5000.00
               MOVE 5000.00 TO PBKM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN LAST ACT DATE IS PAST
      * PR -> RB WHEN LAST ACT DATE IS PAST
      * RB -> LS WHEN LAST ACT DATE IS PAST
      * LS -> CF WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE GRID TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-GRID-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKM-LINE-AMT NOT < WS-BALANCE-GRID-LOW(WS-TX)
                   MOVE WS-BALANCE-GRID-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINTED YTD FEE ON THE PRINTED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3280-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-PRINTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBKM-PRINTED-YTD ROUNDED = PBKM-PRINTED-AMT *
                       PBKM-MARGIN / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 5.00 TO PBKM-PRINTED-YTD
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 3.00 TO PBKM-PRINTED-YTD
               WHEN OTHER
                   COMPUTE PBKM-PRINTED-YTD ROUNDED = PBKM-PRINTED-AMT *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-PRINTED-YTD > 0 AND PBKM-PRINTED-YTD < 10.00
               MOVE 10.00 TO PBKM-PRINTED-YTD
           END-IF
           IF PBKM-PRINTED-YTD > 250.00
               MOVE 250.00 TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKM-INTEREST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-INTEREST-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E884 CUST ID REQUIRED
      * E837 BALANCE YTD NEGATIVE
      * E420 BALANCE YTD OVER 25000
      * E823 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-YTD < 0
                   MOVE 'E837' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-YTD > 25000.00
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN PBKM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-INTEREST.
           IF PBKM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-YTD * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBKM-BALANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD PBKT-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINTED FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3350-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PBKM-PRINTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 5.00 TO PBKM-PRINTED-AMT
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 7.00 TO PBKM-PRINTED-AMT
               WHEN OTHER
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-PRINTED-AMT > 0 AND PBKM-PRINTED-AMT < 1.00
               MOVE 1.00 TO PBKM-PRINTED-AMT
           END-IF
           IF PBKM-PRINTED-AMT > 50.00
               MOVE 50.00 TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-INTEREST-YTD ROUNDED =
                   PBKM-UNPOSTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN NEXT DUE DATE IS PAST
      * ZC -> ZY WHEN NEXT DUE DATE IS PAST
      * ZY -> JX WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3390-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKM-CARRYFWD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = WS-WORK-AMT-2 *
                       PBKM-MARGIN / 200
               WHEN PBKM-BRANCH-CD = 'DF'
                   MOVE 3.00 TO PBKM-CARRYFWD-AMT
               WHEN PBKM-BRANCH-CD = 'NV'
                   MOVE 3.00 TO PBKM-CARRYFWD-AMT
               WHEN OTHER
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = WS-WORK-AMT-2 *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-CARRYFWD-AMT > 0 AND PBKM-CARRYFWD-AMT < 2.00
               MOVE 2.00 TO PBKM-CARRYFWD-AMT
           END-IF
           IF PBKM-CARRYFWD-AMT > 25.00
               MOVE 25.00 TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE GRID TIER FOR THE PRINTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKT-PRINTED-AMT NOT < WS-BALANCE-GRID-LOW(WS-TX)
                   MOVE WS-BALANCE-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKM-INTEREST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-INTEREST-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKT-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PBKT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E097 CUST ID REQUIRED
      * E085 BALANCE YTD NEGATIVE
      * E820 BALANCE YTD OVER 1000000
      * E125 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3440-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E097' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-YTD < 0
                   MOVE 'E085' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-YTD > 1000000.00
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E125' TO WS-REJECT-CODE
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
       3450-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD AMT BY BOOK TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-BOOK-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BOOK-TYPE-CD
                   ADD PBKM-CARRYFWD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BOOK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBKM-UNPOSTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-UNPOSTED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-UNPOSTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3490-LIMIT-PRINTED.
           IF PBKM-PRINTED-YTD < 50.00
               MOVE 50.00 TO PBKM-PRINTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-YTD > 500.00
               MOVE 500.00 TO PBKM-PRINTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 12
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-UNPOSTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY BOOK TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-BOOK-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BOOK-TYPE-CD
                   ADD PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BOOK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE FEE ON THE LINE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3540-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKT-LINE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKT-LINE-AMT ROUNDED = PBKM-LINE-YTD *
                       PBKM-MARGIN / 200
               WHEN PBKM-BOOK-TYPE-CD = 'CH'
                   MOVE 20.00 TO PBKT-LINE-AMT
               WHEN PBKM-BOOK-TYPE-CD = 'HG'
                   MOVE 5.00 TO PBKT-LINE-AMT
               WHEN OTHER
                   COMPUTE PBKT-LINE-AMT ROUNDED = PBKM-LINE-YTD *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKT-LINE-AMT > 0 AND PBKT-LINE-AMT < 10.00
               MOVE 10.00 TO PBKT-LINE-AMT
           END-IF
           IF PBKT-LINE-AMT > 250.00
               MOVE 250.00 TO PBKT-LINE-AMT
           END-IF
           ADD PBKT-LINE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE LINE YTD FEE ON THE PRINTED YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3550-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-LINE-YTD ROUNDED = PBKM-PRINTED-YTD *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-BRANCH-CD = 'DF'
                   MOVE 15.00 TO PBKM-LINE-YTD
               WHEN PBKM-BRANCH-CD = 'NV'
                   MOVE 5.00 TO PBKM-LINE-YTD
               WHEN OTHER
                   COMPUTE PBKM-LINE-YTD ROUNDED = PBKM-PRINTED-YTD *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-LINE-YTD > 0 AND PBKM-LINE-YTD < 2.00
               MOVE 2.00 TO PBKM-LINE-YTD
           END-IF
           IF PBKM-LINE-YTD > 25.00
               MOVE 25.00 TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E535 NAME REQUIRED
      * E096 INTEREST YTD NEGATIVE
      * E957 INTEREST YTD OVER 1000000
      * E996 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3560-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-YTD < 0
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-YTD > 1000000.00
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD AMT BY BOOK TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-BOOK-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BOOK-TYPE-CD
                   ADD PBKT-CARRYFWD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BOOK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-CARRYFWD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE LINE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PBKM-LINE-YTD NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE FEE ON THE CARRYFWD YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3590-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-LINE-AMT ROUNDED = PBKM-CARRYFWD-YTD *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 15.00 TO PBKM-LINE-AMT
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 12.00 TO PBKM-LINE-AMT
               WHEN OTHER
                   COMPUTE PBKM-LINE-AMT ROUNDED = PBKM-CARRYFWD-YTD *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-LINE-AMT > 0 AND PBKM-LINE-AMT < 2.00
               MOVE 2.00 TO PBKM-LINE-AMT
           END-IF
           IF PBKM-LINE-AMT > 50.00
               MOVE 50.00 TO PBKM-LINE-AMT
           END-IF
           ADD PBKM-LINE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E890 MEMO REQUIRED
      * E999 INTEREST AMT NEGATIVE
      * E397 INTEREST AMT OVER 5000
      * E228 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E890' TO WS-REJECT-CODE
               WHEN PBKT-INTEREST-AMT < 0
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN PBKT-INTEREST-AMT > 5000.00
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E228' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOOK TYPE CD:
      * CH -> HG WHEN CLOSE DATE IS PAST
      * HG -> KB WHEN CLOSE DATE IS PAST
      * KB -> KW WHEN CLOSE DATE IS PAST
      * KW -> PA WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HG' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HG'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KB' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KB'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PA' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CARRYFWD YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-CARRYFWD-YTD ROUNDED =
                   PBKT-CARRYFWD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-CARRYFWD-YTD
           END-IF
           ADD PBKM-CARRYFWD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE LINE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-CARRYFWD-AMT ROUNDED =
                   PBKM-LINE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3640-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PBKT-CARRYFWD-AMT *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PBKT-CARRYFWD-AMT *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-BALANCE.
           IF PBKM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-BALANCE-YTD * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-LINE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-INTEREST-AMT ROUNDED =
                   PBKM-UNPOSTED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN OPEN DATE IS PAST
      * PR -> RB WHEN OPEN DATE IS PAST
      * RB -> LS WHEN OPEN DATE IS PAST
      * LS -> CF WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3670-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3740-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E994 CUST ID REQUIRED
      * E093 WORK AMT 2 NEGATIVE
      * E007 WORK AMT 2 OVER 250000
      * E362 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN PBKT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3770-FLAG-PRINTED.
           IF PBKM-PRINTED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-PRINTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3780-FLAG-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-UNPOSTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINTED FEE ON THE UNPOSTED YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3790-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PBKT-PRINTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKT-PRINTED-AMT ROUNDED = PBKM-UNPOSTED-YTD
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO PBKT-PRINTED-AMT
               WHEN PBKT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO PBKT-PRINTED-AMT
               WHEN OTHER
                   COMPUTE PBKT-PRINTED-AMT ROUNDED = PBKM-UNPOSTED-YTD
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKT-PRINTED-AMT > 0 AND PBKT-PRINTED-AMT < 1.00
               MOVE 1.00 TO PBKT-PRINTED-AMT
           END-IF
           IF PBKT-PRINTED-AMT > 150.00
               MOVE 150.00 TO PBKT-PRINTED-AMT
           END-IF
           ADD PBKT-PRINTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOOK TYPE CD:
      * CH -> HG WHEN CLOSE DATE IS PAST
      * HG -> KB WHEN CLOSE DATE IS PAST
      * KB -> KW WHEN CLOSE DATE IS PAST
      * KW -> PA WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HG' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HG'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KB' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KB'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PA' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E195 ADDR REQUIRED
      * E653 PRINTED AMT NEGATIVE
      * E199 PRINTED AMT OVER 10000
      * E004 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-ADDR = SPACES
                   MOVE 'E195' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-AMT < 0
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-AMT > 10000.00
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN MATURITY DATE IS PAST
      * ZC -> ZY WHEN MATURITY DATE IS PAST
      * ZY -> JX WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE GRID TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKM-BALANCE-YTD NOT < WS-BALANCE-GRID-LOW(WS-TX)
                   MOVE WS-BALANCE-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LINE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3860-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKM-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN POST DATE IS PAST
      * NV -> PP WHEN POST DATE IS PAST
      * PP -> SH WHEN POST DATE IS PAST
      * SH -> ZP WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE LINE YTD FEE ON THE UNPOSTED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3890-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE PBKM-LINE-YTD ROUNDED = PBKM-UNPOSTED-AMT *
                       PBKM-FEE-PCT / 200
               WHEN PBKT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO PBKM-LINE-YTD
               WHEN PBKT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO PBKM-LINE-YTD
               WHEN OTHER
                   COMPUTE PBKM-LINE-YTD ROUNDED = PBKM-UNPOSTED-AMT *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-LINE-YTD > 0 AND PBKM-LINE-YTD < 10.00
               MOVE 10.00 TO PBKM-LINE-YTD
           END-IF
           IF PBKM-LINE-YTD > 150.00
               MOVE 150.00 TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3900-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-CARRYFWD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-CARRYFWD-YTD ROUNDED = PBKM-INTEREST-AMT
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-PRINTER-CD = 'CB'
                   MOVE 5.00 TO PBKM-CARRYFWD-YTD
               WHEN PBKM-PRINTER-CD = 'EP'
                   MOVE 10.00 TO PBKM-CARRYFWD-YTD
               WHEN OTHER
                   COMPUTE PBKM-CARRYFWD-YTD ROUNDED = PBKM-INTEREST-AMT
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-CARRYFWD-YTD > 0 AND PBKM-CARRYFWD-YTD < 1.00
               MOVE 1.00 TO PBKM-CARRYFWD-YTD
           END-IF
           IF PBKM-CARRYFWD-YTD > 75.00
               MOVE 75.00 TO PBKM-CARRYFWD-YTD
           END-IF
           ADD PBKM-CARRYFWD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE PRINTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PBKM-PRINTED-YTD NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKT-LINE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKT-LINE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3950-FLAG-UNPOSTED.
           IF PBKM-UNPOSTED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-UNPOSTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3960-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3990-LIMIT-CARRYFWD.
           IF PBKT-CARRYFWD-AMT < 25.00
               MOVE 25.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-CARRYFWD-AMT > 1000.00
               MOVE 1000.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-REGION-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED BAND TIER FOR THE UNPOSTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-BAND-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKM-UNPOSTED-AMT NOT < WS-PRINTED-BAND-LOW(WS-TX)
                   MOVE WS-PRINTED-BAND-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-PBSTAT.
           INITIALIZE PBSTAT-PARMS
           MOVE PBKM-UNPOSTED-YTD
             TO PBSTAT-UNPOSTED-YTD
           MOVE PBKM-BALANCE-AMT
             TO PBSTAT-BALANCE-AMT
           MOVE PBKM-LINE-YTD
             TO PBSTAT-LINE-YTD
           MOVE PBKM-CARRYFWD-YTD
             TO PBSTAT-CARRYFWD-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBSTAT-UNPOSTED-AMT
           MOVE PBKM-LINE-AMT
             TO PBSTAT-LINE-AMT
           MOVE PBKM-CLOSE-DATE
             TO PBSTAT-CLOSE-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBSTAT-EFFECTIVE-DATE
           MOVE PBKM-NEXT-DUE-DATE
             TO PBSTAT-NEXT-DUE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBSTAT-NEXT-REVIEW-DATE
           MOVE PBKM-BOOK-TYPE-CD
             TO PBSTAT-BOOK-TYPE-CD
           MOVE PBKM-PRINTER-CD
             TO PBSTAT-PRINTER-CD
           MOVE PBKM-REGION-CD
             TO PBSTAT-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBSTAT-SOURCE-CD
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBSTAT-NOTICE-SENT-FLAG
           MOVE PBKM-REVIEW-FLAG
             TO PBSTAT-REVIEW-FLAG
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBSTAT-AUTO-RENEW-FLAG
           MOVE PBKM-RETRY-CNT
             TO PBSTAT-RETRY-CNT
           MOVE PBKM-CYCLE-CNT
             TO PBSTAT-CYCLE-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBSTAT-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBSTAT-MARGIN
           MOVE PBKM-CONTACT
             TO PBSTAT-CONTACT
           MOVE PBKM-CUST-ID
             TO PBSTAT-CUST-ID
           CALL 'PBSTAT' USING PBSTAT-PARMS
           EVALUATE PBSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBSTAT-LINE-YTD
                     TO PBKM-LINE-YTD
                   MOVE PBSTAT-CARRYFWD-YTD
                     TO PBKM-CARRYFWD-YTD
                   MOVE PBSTAT-NOTICE-SENT-FLAG
                     TO PBKM-NOTICE-SENT-FLAG
                   MOVE PBSTAT-REVIEW-FLAG
                     TO PBKM-REVIEW-FLAG
                   MOVE PBSTAT-AUTO-RENEW-FLAG
                     TO PBKM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PBSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-PBDTE.
           INITIALIZE PBDTE-PARMS
           MOVE PBKM-PRINTED-YTD
             TO PBDTE-PRINTED-YTD
           MOVE PBKM-INTEREST-YTD
             TO PBDTE-INTEREST-YTD
           MOVE PBKM-UNPOSTED-YTD
             TO PBDTE-UNPOSTED-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBDTE-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-AMT
             TO PBDTE-CARRYFWD-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBDTE-CARRYFWD-YTD
           MOVE PBKM-NEXT-DUE-DATE
             TO PBDTE-NEXT-DUE-DATE
           MOVE PBKM-MATURITY-DATE
             TO PBDTE-MATURITY-DATE
           MOVE PBKM-OPEN-DATE
             TO PBDTE-OPEN-DATE
           MOVE PBKM-CLOSE-DATE
             TO PBDTE-CLOSE-DATE
           MOVE PBKM-REGION-CD
             TO PBDTE-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBDTE-SOURCE-CD
           MOVE PBKM-BRANCH-CD
             TO PBDTE-BRANCH-CD
           MOVE PBKM-PRINTER-CD
             TO PBDTE-PRINTER-CD
           MOVE PBKM-REVIEW-FLAG
             TO PBDTE-REVIEW-FLAG
           MOVE PBKM-EXEMPT-FLAG
             TO PBDTE-EXEMPT-FLAG
           MOVE PBKM-SUSPEND-FLAG
             TO PBDTE-SUSPEND-FLAG
           MOVE PBKM-CYCLE-CNT
             TO PBDTE-CYCLE-CNT
           MOVE PBKM-RETRY-CNT
             TO PBDTE-RETRY-CNT
           MOVE PBKM-MARGIN
             TO PBDTE-MARGIN
           MOVE PBKM-FEE-PCT
             TO PBDTE-FEE-PCT
           MOVE PBKM-CUST-ID
             TO PBDTE-CUST-ID
           MOVE PBKM-NAME
             TO PBDTE-NAME
           CALL 'PBDTE' USING PBDTE-PARMS
           EVALUATE PBDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBDTE-PRINTED-YTD
                     TO PBKM-PRINTED-YTD
                   MOVE PBDTE-INTEREST-YTD
                     TO PBKM-INTEREST-YTD
                   MOVE PBDTE-UNPOSTED-YTD
                     TO PBKM-UNPOSTED-YTD
                   MOVE PBDTE-UNPOSTED-AMT
                     TO PBKM-UNPOSTED-AMT
                   MOVE PBDTE-CARRYFWD-YTD
                     TO PBKM-CARRYFWD-YTD
                   MOVE PBDTE-REVIEW-FLAG
                     TO PBKM-REVIEW-FLAG
                   MOVE PBDTE-EXEMPT-FLAG
                     TO PBKM-EXEMPT-FLAG
                   MOVE PBDTE-SUSPEND-FLAG
                     TO PBKM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-PBDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PBKT-KEY TO RD-KEY
           MOVE PBKT-TRAN-CD TO RD-CODE
           MOVE PBKT-PRINTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PBKT-KEY TO RD-KEY
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
           DISPLAY 'PBKCONV READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PBKCONV' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
