      *================================================================*
      * PROGRAM  : FNNRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : FOREIGN BANKNOTES - RECONCILIATION. READS THE FXN   *
      *            TRANSACTIONS, UPDATES THE FXN MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FOREIGN CURRENCY BANKNOTE       *
      *            ORDERS, BUYBACK AND MARGIN                          *
      * INPUT    : FXNMST (KSDS), FXNTRN, BANKCTL                      *
      * OUTPUT   : FXNOUT, FNNRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, FNSTAT, FNELG           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-18 SKN  ORIGINAL                                       *
      * 1997-01-18 TWH  SHIPPING TABLE EXPANDED INC4003                *
      * 2000-05-08 CRP  FOREIGN EXCEPTIONS TO OPS QUEUE REQ3823        *
      * 2001-06-09 EWS  MARGIN EXCEPTIONS TO OPS QUEUE CHG0288         *
      * 2002-03-25 PQR  USD FEE SCHEDULE UPDATE PRB2594                *
      * 2003-11-06 JLM  MARGIN STATUS CODES ADDED CHG5301              *
      * 2004-04-03 LMB  MARGIN STATUS CODES ADDED PRB5398              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNNRCON.
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
           SELECT REPORT-FILE ASSIGN TO "FNNRCRPT"
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
                                           VALUE 23.000000.
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
      * WS-MARGIN-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MARGIN-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000166500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000743000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000895500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000148500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000201000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000047500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000589000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022300000000493000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027300000000618500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027350000000496000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027450000000379000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032450000000653500'.
       01  WS-MARGIN-BAND-TBL REDEFINES WS-MARGIN-BAND-VALUES.
           05  WS-MARGIN-BAND-ROW OCCURS 12 TIMES.
               10  WS-MARGIN-BAND-LOW      PIC 9(09)V99.
               10  WS-MARGIN-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FOREIGN-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FOREIGN-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000886000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000221500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000499500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000106000'.
       01  WS-FOREIGN-GRID-TBL REDEFINES WS-FOREIGN-GRID-VALUES.
           05  WS-FOREIGN-GRID-ROW OCCURS 4 TIMES.
               10  WS-FOREIGN-GRID-LOW     PIC 9(09)V99.
               10  WS-FOREIGN-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUYBACK-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000581000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000802000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000276500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000534000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000031000'.
       01  WS-BUYBACK-BAND-TBL REDEFINES WS-BUYBACK-BAND-VALUES.
           05  WS-BUYBACK-BAND-ROW OCCURS 5 TIMES.
               10  WS-BUYBACK-BAND-LOW     PIC 9(09)V99.
               10  WS-BUYBACK-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  FNNRCON  FOREIGN BANKNOTES'.
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
       COPY FNSTATP.
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
               PERFORM 3000-LIMIT-USD
               PERFORM 3010-FLAG-BUYBACK
               PERFORM 3020-FLAG-USD
               PERFORM 3030-ACCRUE-SHIPPING
               PERFORM 3040-DESCRIBE-ORDER-TYPE
               PERFORM 3050-ACCUM-TYPE
               PERFORM 3060-ACCRUE-USD
               PERFORM 3070-DESCRIBE-ORDER-TYPE
               PERFORM 3080-FLAG-USD
               PERFORM 3090-DESCRIBE-TRAN
               PERFORM 3100-SPLIT-SHIPPING
               PERFORM 3110-FLAG-BUYBACK
               PERFORM 3120-AGE-CLOSE
               PERFORM 3130-COUNT-EXCEPT
               PERFORM 3140-LIMIT-ITEM
               PERFORM 3150-PRORATE-MARGIN
               PERFORM 3160-LIMIT-USD
               PERFORM 3170-ACCUM-TYPE
               PERFORM 3180-ACCRUE-FOREIGN
               PERFORM 3190-COUNT-NOTICE
               PERFORM 3200-ACCUM-STATUS
               PERFORM 3210-PRORATE-BUYBACK
               PERFORM 3220-COUNT-NOTICE
               PERFORM 3230-SPLIT-MARGIN
               PERFORM 3240-COUNT-EXCEPT
               PERFORM 3250-FLAG-MARGIN
               PERFORM 3260-SET-ORDER-TYPE
               PERFORM 3270-AGE-EXPIRY
               PERFORM 3280-ASSESS-NOTICE-SENT
               PERFORM 3290-DESCRIBE-TYPE
               PERFORM 3300-SET-ORDER-TYPE
               PERFORM 3310-SPLIT-FOREIGN
               PERFORM 3320-EDIT-NAME
               PERFORM 3330-EDIT-NAME
               PERFORM 3340-ACCRUE-BUYBACK
               PERFORM 3350-AGE-EXPIRY
               PERFORM 3360-LIMIT-MARGIN
               PERFORM 3370-ACCUM-TRAN
               PERFORM 3380-AGE-POST
               PERFORM 3390-COUNT-CYCLE
               PERFORM 3400-PRORATE-SHIPPING
               PERFORM 3410-SPLIT-MARGIN
               PERFORM 3420-ACCUM-SOURCE
               PERFORM 3430-FLAG-SHIPPING
               PERFORM 3440-DESCRIBE-STATUS
               PERFORM 3450-PRORATE-FOREIGN
               PERFORM 3460-SET-TYPE
               PERFORM 3470-ASSESS-OVERRIDE
               PERFORM 3480-PRORATE-FOREIGN
               PERFORM 3490-LIMIT-FOREIGN
               PERFORM 3500-PRORATE-BUYBACK
               PERFORM 3510-ACCUM-TRAN
               PERFORM 3520-LIMIT-MARGIN
               PERFORM 3530-DESCRIBE-CURRENCY
               PERFORM 3540-FLAG-MARGIN
               PERFORM 3550-COUNT-CYCLE
               PERFORM 3560-SET-CHANNEL
               PERFORM 3570-EDIT-CONTACT
               PERFORM 3580-SET-ORDER-TYPE
               PERFORM 3590-SPLIT-FOREIGN
               PERFORM 3600-ASSESS-NOTICE-SENT
               PERFORM 3610-SPLIT-MARGIN
               PERFORM 3620-SET-CURRENCY
               PERFORM 3630-DESCRIBE-TRAN
               PERFORM 3640-LIMIT-FOREIGN
               PERFORM 3650-AGE-EFF
               PERFORM 3660-COUNT-CYCLE
               PERFORM 3670-SPLIT-FOREIGN
               PERFORM 3680-EDIT-CUST-ID
               PERFORM 3690-PRORATE-SHIPPING
               PERFORM 3700-ACCRUE-BUYBACK
               PERFORM 3710-SELECT-RATE
               PERFORM 3720-EDIT-OPERATOR
               PERFORM 3730-SELECT-MARGIN
               PERFORM 3740-AGE-LAST-MAINT
               PERFORM 3750-ACCRUE-MARGIN
               PERFORM 3760-EDIT-ADDR
               PERFORM 3770-LIMIT-BUYBACK
               PERFORM 3780-SPLIT-FOREIGN
               PERFORM 3790-ACCUM-BRANCH
               PERFORM 3800-DESCRIBE-SOURCE
               PERFORM 3810-FLAG-SHIPPING
               PERFORM 3820-FLAG-MARGIN
               PERFORM 3830-SPLIT-USD
               PERFORM 3840-ACCRUE-USD
               PERFORM 3850-SELECT-MARGIN
               PERFORM 3860-ACCRUE-FOREIGN
               PERFORM 3870-SELECT-MARGIN
               PERFORM 3880-SET-CHANNEL
               PERFORM 3890-PRORATE-SHIPPING
               PERFORM 3900-CHECK-OPEN
               PERFORM 3910-EDIT-CUST-ID
               PERFORM 3920-SPLIT-SHIPPING
               PERFORM 3930-FLAG-ITEM
               PERFORM 3940-DESCRIBE-CURRENCY
               PERFORM 3950-COUNT-PAYMENT
               PERFORM 3960-FLAG-USD
               PERFORM 3970-SPLIT-BUYBACK
               PERFORM 7040-CALL-FNSTAT
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
      * USD AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-USD.
           IF FXNM-USD-AMT < 100.00
               MOVE 100.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 5000.00
               MOVE 5000.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3010-FLAG-BUYBACK.
           IF FXNM-BUYBACK-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3020-FLAG-USD.
           IF FXNT-USD-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNT-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-TYPE-CD
                   ADD FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-USD.
           IF FXNM-USD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-USD-YTD * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-FOREIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3080-FLAG-USD.
           IF FXNT-USD-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3100-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FXNM-SHIPPING-YTD / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-SHIPPING-YTD - (WS-WORK-AMT *
               12)
           COMPUTE FXNT-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-BUYBACK.
           IF FXNM-BUYBACK-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-CLOSE.
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
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3140-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN YTD.
      *----------------------------------------------------------------*
       3150-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-MARGIN-YTD ROUNDED =
                   FXNM-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-MARGIN-YTD
           END-IF
           ADD FXNM-MARGIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-USD.
           IF FXNM-USD-AMT < 25.00
               MOVE 25.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 1000.00
               MOVE 1000.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-TYPE-CD
                   ADD FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNT-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO FXNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       3210-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-BUYBACK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE FXNT-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3250-FLAG-MARGIN.
           IF FXNM-MARGIN-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN LAST ACT DATE IS PAST
      * KZV -> PHV WHEN LAST ACT DATE IS PAST
      * PHV -> PZZ WHEN LAST ACT DATE IS PAST
      * PZZ -> WMC WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-EXPIRY.
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
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE USD AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3280-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNT-USD-AMT *
                       FXNM-RATE / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 5.00 TO FXNM-SHIPPING-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 20.00 TO FXNM-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNT-USD-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-SHIPPING-AMT > 0 AND FXNM-SHIPPING-AMT < 10.00
               MOVE 10.00 TO FXNM-SHIPPING-AMT
           END-IF
           IF FXNM-SHIPPING-AMT > 50.00
               MOVE 50.00 TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN EFFECTIVE DATE IS PAST
      * KZV -> PHV WHEN EFFECTIVE DATE IS PAST
      * PHV -> PZZ WHEN EFFECTIVE DATE IS PAST
      * PZZ -> WMC WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNM-USD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E296 NAME REQUIRED
      * E590 SHIPPING YTD NEGATIVE
      * E104 SHIPPING YTD OVER 10000
      * E158 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD < 0
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD > 10000.00
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E158' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E263 NAME REQUIRED
      * E087 BUYBACK YTD NEGATIVE
      * E174 BUYBACK YTD OVER 250000
      * E438 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-NAME = SPACES
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD < 0
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD > 250000.00
                   MOVE 'E174' TO WS-REJECT-CODE
               WHEN FXNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E438' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO FXNM-SHIPPING-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-EXPIRY.
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
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARGIN YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-MARGIN.
           IF FXNM-MARGIN-YTD < 100.00
               MOVE 100.00 TO FXNM-MARGIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-YTD > 10000.00
               MOVE 10000.00 TO FXNM-MARGIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-POST.
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
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       3400-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   FXNT-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNM-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3430-FLAG-SHIPPING.
           IF FXNM-SHIPPING-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-SHIPPING-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ESS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XDE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XFD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN CLOSE DATE IS PAST
      * FFU -> JNR WHEN CLOSE DATE IS PAST
      * JNR -> UGS WHEN CLOSE DATE IS PAST
      * UGS -> XUM WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE USD AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3470-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 20.00 TO FXNT-BUYBACK-AMT
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 3.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-BUYBACK-AMT > 0 AND FXNT-BUYBACK-AMT < 2.00
               MOVE 2.00 TO FXNT-BUYBACK-AMT
           END-IF
           IF FXNT-BUYBACK-AMT > 35.00
               MOVE 35.00 TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3480-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   FXNM-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3490-LIMIT-FOREIGN.
           IF FXNT-FOREIGN-AMT < 50.00
               MOVE 50.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-FOREIGN-AMT > 1000.00
               MOVE 1000.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-BUYBACK-AMT ROUNDED =
                   FXNM-BUYBACK-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-MARGIN.
           IF FXNM-MARGIN-AMT < 100.00
               MOVE 100.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-MARGIN.
           IF FXNT-MARGIN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E109 CONTACT REQUIRED
      * E672 WORK AMT 2 NEGATIVE
      * E415 WORK AMT 2 OVER 5000
      * E302 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CONTACT = SPACES
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E415' TO WS-REJECT-CODE
               WHEN FXNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN LAST PAY DATE IS PAST
      * KZV -> PHV WHEN LAST PAY DATE IS PAST
      * PHV -> PZZ WHEN LAST PAY DATE IS PAST
      * PZZ -> WMC WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-ORDER-TYPE.
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
      * SPLIT FOREIGN YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-YTD / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE SHIPPING AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3600-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-SHIPPING-AMT
                       * FXNM-RATE / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 5.00 TO FXNM-SHIPPING-YTD
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 12.00 TO FXNM-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-SHIPPING-AMT
                       * FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-SHIPPING-YTD > 0 AND FXNM-SHIPPING-YTD < 10.00
               MOVE 10.00 TO FXNM-SHIPPING-YTD
           END-IF
           IF FXNM-SHIPPING-YTD > 35.00
               MOVE 35.00 TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNT-MARGIN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNT-MARGIN-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-BUYBACK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN EFF DATE IS PAST
      * E -> G WHEN EFF DATE IS PAST
      * G -> P WHEN EFF DATE IS PAST
      * P -> W WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-CURRENCY.
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
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-FOREIGN.
           IF FXNT-FOREIGN-AMT < 25.00
               MOVE 25.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-FOREIGN-AMT > 5000.00
               MOVE 5000.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-EFF.
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
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNM-FOREIGN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E978 CUST ID REQUIRED
      * E138 SHIPPING AMT NEGATIVE
      * E201 SHIPPING AMT OVER 1000000
      * E956 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-AMT < 0
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-AMT > 1000000.00
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN FXNM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       3690-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-BUYBACK.
           IF FXNM-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNT-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-BAND-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-MARGIN-BAND-LOW(WS-TX)
                   MOVE WS-MARGIN-BAND-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E455 OPERATOR REQUIRED
      * E817 SHIPPING AMT NEGATIVE
      * E421 SHIPPING AMT OVER 99999
      * E534 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-AMT < 0
                   MOVE 'E817' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-AMT > 99999.00
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN FXNM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-BAND-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-MARGIN-BAND-LOW(WS-TX)
                   MOVE WS-MARGIN-BAND-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-LAST-MAINT.
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
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-MARGIN.
           IF FXNM-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-MARGIN-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-SHIPPING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E288 ADDR REQUIRED
      * E340 WORK AMT 2 NEGATIVE
      * E199 WORK AMT 2 OVER 10000
      * E286 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-ADDR = SPACES
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-BUYBACK.
           IF FXNT-BUYBACK-AMT < 10.00
               MOVE 10.00 TO FXNT-BUYBACK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-BUYBACK-AMT > 10000.00
               MOVE 10000.00 TO FXNT-BUYBACK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE FXNT-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-BRANCH-CD
                   ADD FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPPING AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3810-FLAG-SHIPPING.
           IF FXNM-SHIPPING-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-SHIPPING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3820-FLAG-MARGIN.
           IF FXNM-MARGIN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNM-USD-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-USD-YTD - (WS-WORK-AMT * 3)
           COMPUTE FXNT-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-USD.
           IF FXNM-USD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-USD-YTD * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE FOREIGN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF FXNM-FOREIGN-YTD NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-USD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF FXNT-USD-AMT NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
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
       3880-SET-CHANNEL.
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
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E681 CUST ID REQUIRED
      * E680 SHIPPING AMT NEGATIVE
      * E868 SHIPPING AMT OVER 25000
      * E178 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-AMT < 0
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-AMT > 25000.00
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN FXNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E178' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNM-SHIPPING-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3930-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FXNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3960-FLAG-USD.
           IF FXNT-USD-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNT-BUYBACK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-BUYBACK-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNT-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

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
                   MOVE FNSTAT-BUYBACK-AMT
                     TO FXNM-BUYBACK-AMT
                   MOVE FNSTAT-MARGIN-AMT
                     TO FXNM-MARGIN-AMT
                   MOVE FNSTAT-SHIPPING-YTD
                     TO FXNM-SHIPPING-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-FNSTAT' TO ABND-PARAGRAPH
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
                   MOVE FNELG-FOREIGN-AMT
                     TO FXNM-FOREIGN-AMT
                   MOVE FNELG-SHIPPING-AMT
                     TO FXNM-SHIPPING-AMT
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
           DISPLAY 'FNNRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'FNNRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
