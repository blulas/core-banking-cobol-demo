      *================================================================*
      * PROGRAM  : TRSAGE                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PERSONAL TRUST - AGING. READS THE TRS TRANSACTIONS, *
      *            UPDATES THE TRS MASTER AND PRODUCES THE CONTROL     *
      *            REPORT. PERSONAL TRUST ACCOUNTING, INCOME           *
      *            DISTRIBUTION AND TRUSTEE FEES                       *
      * INPUT    : TRSMST (KSDS), TRSTRN, BANKCTL                      *
      * OUTPUT   : TRSOUT, TRSAGRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TRPEN, TRLIM, TRVAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-06 GAF  ORIGINAL                                       *
      * 1993-01-10 LMB  TRUSTEE-FEE STATUS CODES ADDED CHG4533         *
      * 1995-01-11 GAF  PRINCIPAL CALCULATION CORRECTED REQ4750        *
      * 1997-05-22 EWS  AUDIT FINDING - PRINCIPAL CONTROL TOTALS PRB545*
      * 1999-07-13 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-10-16 CRP  TAX ROUNDING - HALF UP CHG2185                 *
      * 2007-09-07 DPS  TRUSTEE-FEE ROUNDING - HALF UP CHG6001         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRSAGE.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TRSMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TRSM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TRSTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TRSOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TRSAGRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TRSMST.
       FD  TRAN-FILE.
       COPY TRSTRN.
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
      * WS-PRINCIPA-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000822500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000233000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000638000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000215000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000727000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020350000000604000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020850000000875000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000540000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022350000000102000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023350000000084500'.
       01  WS-PRINCIPA-BAND-TBL REDEFINES WS-PRINCIPA-BAND-VALUES.
           05  WS-PRINCIPA-BAND-ROW OCCURS 10 TIMES.
               10  WS-PRINCIPA-BAND-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INCOME-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INCOME-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000160000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000417500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000752500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000311500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000344500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000686000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000662000'.
       01  WS-INCOME-BAND-TBL REDEFINES WS-INCOME-BAND-VALUES.
           05  WS-INCOME-BAND-ROW OCCURS 7 TIMES.
               10  WS-INCOME-BAND-LOW      PIC 9(09)V99.
               10  WS-INCOME-BAND-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TRSAGE  PERSONAL TRUST'.
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
       COPY TRPENP.
       COPY TRLIMP.
       COPY TRVALP.

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
               MOVE 'TRSMST' TO ABND-FILE-NAME
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
           MOVE TRST-KEY TO TRSM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCRUE-PRINCIPAL
               PERFORM 3010-SELECT-MARGIN
               PERFORM 3020-EDIT-CUST-ID
               PERFORM 3030-COUNT-NOTICE
               PERFORM 3040-SPLIT-TRUSTEE-FEE
               PERFORM 3050-ACCUM-TRAN
               PERFORM 3060-ACCUM-CYCLE
               PERFORM 3070-FLAG-PRINCIPAL
               PERFORM 3080-LIMIT-TRUSTEE-FEE
               PERFORM 3090-LIMIT-MARKET
               PERFORM 3100-CHECK-LAST-STMT
               PERFORM 3110-AGE-OPEN
               PERFORM 3120-EDIT-OPERATOR
               PERFORM 3130-DESCRIBE-TRUST-TYPE
               PERFORM 3140-SPLIT-DISTRIB
               PERFORM 3150-SELECT-MARGIN
               PERFORM 3160-PRORATE-MARKET
               PERFORM 3170-FLAG-DISTRIB
               PERFORM 3180-EDIT-ADDR
               PERFORM 3190-AGE-LAST-STMT
               PERFORM 3200-AGE-MATURITY
               PERFORM 3210-LIMIT-TRUSTEE-FEE
               PERFORM 3220-ACCRUE-TRUSTEE-FEE
               PERFORM 3230-ASSESS-SUSPEND
               PERFORM 3240-EDIT-REJECT
               PERFORM 3250-AGE-POST
               PERFORM 3260-PRORATE-MARKET
               PERFORM 3270-CHECK-MATURITY
               PERFORM 3280-ACCRUE-PRINCIPAL
               PERFORM 3290-SELECT-DISCOUNT
               PERFORM 3300-EDIT-DESC
               PERFORM 3310-ASSESS-REVIEW
               PERFORM 3320-EDIT-REJECT
               PERFORM 3330-SELECT-RATE
               PERFORM 3340-SPLIT-TRUSTEE-FEE
               PERFORM 3350-PRORATE-MARKET
               PERFORM 3360-SPLIT-TAX
               PERFORM 3370-PRORATE-TRUSTEE-FEE
               PERFORM 3380-ASSESS-REVERSAL
               PERFORM 3390-AGE-OPEN
               PERFORM 3400-COUNT-NOTICE
               PERFORM 3410-AGE-LAST-PAY
               PERFORM 3420-FLAG-INCOME
               PERFORM 3430-LIMIT-INCOME
               PERFORM 3440-CHECK-CLOSE
               PERFORM 3450-FLAG-ITEM
               PERFORM 3460-PRORATE-PRINCIPAL
               PERFORM 3470-SELECT-MARGIN
               PERFORM 3480-ASSESS-STOP
               PERFORM 3490-SET-TRUST-TYPE
               PERFORM 3500-ACCUM-REGION
               PERFORM 3510-FLAG-DISTRIB
               PERFORM 3520-SET-CHANNEL
               PERFORM 3530-AGE-LAST-PAY
               PERFORM 3540-DESCRIBE-DIST-FREQ
               PERFORM 3550-SET-REGION
               PERFORM 3560-ACCUM-REASON
               PERFORM 3570-SELECT-RATE
               PERFORM 3580-SELECT-MARGIN
               PERFORM 3590-DESCRIBE-CHANNEL
               PERFORM 3600-SELECT-RATE
               PERFORM 3610-EDIT-REF-NO
               PERFORM 3620-AGE-EFFECTIVE
               PERFORM 3630-SET-STATUS
               PERFORM 3640-EDIT-REJECT
               PERFORM 3650-SPLIT-TRUSTEE-FEE
               PERFORM 3660-SPLIT-PRINCIPAL
               PERFORM 3670-EDIT-REJECT
               PERFORM 3680-LIMIT-TRUSTEE-FEE
               PERFORM 3690-EDIT-ADDR
               PERFORM 3700-AGE-LAST-PAY
               PERFORM 3710-LIMIT-INCOME
               PERFORM 3720-SPLIT-INCOME
               PERFORM 3730-CHECK-OPEN
               PERFORM 3740-EDIT-DESC
               PERFORM 3750-ACCUM-REASON
               PERFORM 3760-ACCUM-TRAN
               PERFORM 3770-EDIT-MEMO
               PERFORM 3780-AGE-CLOSE
               PERFORM 3790-ASSESS-EXEMPT
               PERFORM 3800-SET-REGION
               PERFORM 3810-ACCRUE-DISTRIB
               PERFORM 3820-ASSESS-EXEMPT
               PERFORM 3830-SPLIT-INCOME
               PERFORM 3840-LIMIT-TRUSTEE-FEE
               PERFORM 3850-SET-DIST-FREQ
               PERFORM 3860-PRORATE-INCOME
               PERFORM 3870-AGE-EFFECTIVE
               PERFORM 3880-CHECK-OPEN
               PERFORM 3890-SELECT-RATE
               PERFORM 3900-DESCRIBE-TRAN
               PERFORM 3910-SET-TRUST-TYPE
               PERFORM 3920-LIMIT-MARKET
               PERFORM 3930-CHECK-MATURITY
               PERFORM 3940-COUNT-PAYMENT
               PERFORM 3950-FLAG-TRUSTEE-FEE
               PERFORM 3960-DESCRIBE-STATUS
               PERFORM 3970-SET-TRUST-TYPE
               PERFORM 3980-COUNT-SEQ
               PERFORM 3990-ACCRUE-INCOME
               PERFORM 4000-AGE-EFFECTIVE
               PERFORM 4010-SPLIT-TRUSTEE-FEE
               PERFORM 4020-AGE-NOTICE
               PERFORM 4030-SPLIT-TAX
               PERFORM 4040-DESCRIBE-REGION
               PERFORM 4050-ACCRUE-TRUSTEE-FEE
               PERFORM 4060-FLAG-MARKET
               PERFORM 4070-PRORATE-MARKET
               PERFORM 4080-SELECT-DISCOUNT
               PERFORM 4090-ASSESS-STOP
               PERFORM 4100-SELECT-MARGIN
               PERFORM 4110-ACCUM-REGION
               PERFORM 4120-ASSESS-NOTICE-SENT
               PERFORM 4130-CHECK-OPEN
               PERFORM 4140-AGE-CLOSE
               PERFORM 4150-DESCRIBE-CYCLE
               PERFORM 4160-CHECK-LAST-STMT
               PERFORM 4170-SET-TRAN
               PERFORM 4180-LIMIT-TAX
               PERFORM 4190-CHECK-LAST-MAINT
               PERFORM 4200-EDIT-DESC
               PERFORM 4210-ACCRUE-TRUSTEE-FEE
               PERFORM 4220-AGE-POST
               PERFORM 4230-SELECT-RATE
               PERFORM 7010-CALL-TRPEN
               PERFORM 7000-CALL-TRLIM
               PERFORM 7020-CALL-TRVAL
               REWRITE TRS-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TRSMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TRS-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-PRINCIPAL-AMT * TRSM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E066 CUST ID REQUIRED
      * E134 INCOME AMT NEGATIVE
      * E742 INCOME AMT OVER 99999
      * E440 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CUST-ID = SPACES
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-AMT < 0
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-AMT > 99999.00
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN TRSM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRST-TRUSTEE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRST-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TRSM-INCOME-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRST-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3060-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRST-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3070-FLAG-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRST-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3080-LIMIT-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT < 50.00
               MOVE 50.00 TO TRSM-TRUSTEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TRUSTEE-FEE-AMT > 500.00
               MOVE 500.00 TO TRSM-TRUSTEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-MARKET.
           IF TRSM-MARKET-YTD < 100.00
               MOVE 100.00 TO TRSM-MARKET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-YTD > 1000.00
               MOVE 1000.00 TO TRSM-MARKET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E582 OPERATOR REQUIRED
      * E647 INCOME YTD NEGATIVE
      * E404 INCOME YTD OVER 50000
      * E047 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-OPERATOR = SPACES
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-YTD < 0
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-YTD > 50000.00
                   MOVE 'E404' TO WS-REJECT-CODE
               WHEN TRST-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRUST TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MNG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VFN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XPL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRSM-DISTRIB-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-DISTRIB-YTD - (WS-WORK-AMT * 2)
           COMPUTE TRSM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BAND TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-TAX-YTD NOT < WS-PRINCIPA-BAND-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3170-FLAG-DISTRIB.
           IF TRSM-DISTRIB-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E436 ADDR REQUIRED
      * E726 PRINCIPAL YTD NEGATIVE
      * E154 PRINCIPAL YTD OVER 1000000
      * E068 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-ADDR = SPACES
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-YTD < 0
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN TRSM-PRINCIPAL-YTD > 1000000.00
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRUSTEE FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3210-LIMIT-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD < 10.00
               MOVE 10.00 TO TRSM-TRUSTEE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 5000.00
               MOVE 5000.00 TO TRSM-TRUSTEE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-YTD * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3230-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRSM-INCOME-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRSM-INCOME-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-RATE / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 20.00 TO TRSM-INCOME-YTD
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 12.00 TO TRSM-INCOME-YTD
               WHEN OTHER
                   COMPUTE TRSM-INCOME-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-INCOME-YTD > 0 AND TRSM-INCOME-YTD < 5.00
               MOVE 5.00 TO TRSM-INCOME-YTD
           END-IF
           IF TRSM-INCOME-YTD > 35.00
               MOVE 35.00 TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E997 REJECT CD REQUIRED
      * E894 WORK AMT NEGATIVE
      * E703 WORK AMT OVER 25000
      * E940 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E703' TO WS-REJECT-CODE
               WHEN TRSM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0 AND TRST-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       3260-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   TRSM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRSM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRSM-INCOME-YTD NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E205 DESC REQUIRED
      * E278 WORK AMT NEGATIVE
      * E891 WORK AMT OVER 25000
      * E546 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E205' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E278' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE YTD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3310-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRSM-DISTRIB-AMT * TRSM-MARGIN / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 15.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 20.00 TO TRSM-TRUSTEE-FEE-YTD
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                       TRSM-DISTRIB-AMT * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-YTD > 0 AND TRSM-TRUSTEE-FEE-YTD < 1.00
               MOVE 1.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           IF TRSM-TRUSTEE-FEE-YTD > 100.00
               MOVE 100.00 TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E192 REJECT CD REQUIRED
      * E340 TAX YTD NEGATIVE
      * E698 TAX YTD OVER 10000
      * E848 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD < 0
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD > 10000.00
                   MOVE 'E698' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BAND TIER FOR THE MARKET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-MARKET-AMT NOT < WS-PRINCIPA-BAND-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRST-TRUSTEE-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRST-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TRSM-INCOME-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       3350-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   TRSM-DISTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRSM-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE TRSM-INCOME-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE YTD.
      *----------------------------------------------------------------*
       3370-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                   TRSM-DISTRIB-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE MARKET YTD FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3380-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-MARKET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-MARKET-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-RATE / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 10.00 TO TRSM-MARKET-YTD
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 10.00 TO TRSM-MARKET-YTD
               WHEN OTHER
                   COMPUTE TRSM-MARKET-YTD ROUNDED =
                       TRSM-TRUSTEE-FEE-AMT * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-MARKET-YTD > 0 AND TRSM-MARKET-YTD < 5.00
               MOVE 5.00 TO TRSM-MARKET-YTD
           END-IF
           IF TRSM-MARKET-YTD > 75.00
               MOVE 75.00 TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-INCOME.
           IF TRSM-INCOME-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-INCOME-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-INCOME.
           IF TRST-INCOME-AMT < 100.00
               MOVE 100.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-INCOME-AMT > 10000.00
               MOVE 10000.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3450-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARKET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3460-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-PRINCIPAL-AMT ROUNDED =
                   TRSM-MARKET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BAND TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-PRINCIPAL-YTD NOT < WS-PRINCIPA-BAND-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MARKET YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3480-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-MARKET-YTD *
                       TRSM-MARGIN / 200
               WHEN TRSM-REASON-CD = 'H'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN TRSM-REASON-CD = 'M'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TRSM-MARKET-YTD *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN MATURITY DATE IS PAST
      * MNG -> VFN WHEN MATURITY DATE IS PAST
      * VFN -> XPL WHEN MATURITY DATE IS PAST
      * XPL -> FLJ WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REGION-CD
                   ADD TRSM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3510-FLAG-DISTRIB.
           IF TRSM-DISTRIB-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN MATURITY DATE IS PAST
      * D -> N WHEN MATURITY DATE IS PAST
      * N -> T WHEN MATURITY DATE IS PAST
      * T -> A WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BAND TIER FOR THE INCOME AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRST-INCOME-AMT NOT < WS-PRINCIPA-BAND-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRST-PRINCIPAL-AMT NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE MARKET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRSM-MARKET-YTD NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E882 REF NO REQUIRED
      * E266 DISTRIB AMT NEGATIVE
      * E849 DISTRIB AMT OVER 25000
      * E428 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-REF-NO = SPACES
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E266' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 25000.00
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3620-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN LAST PAY DATE IS PAST
      * GG -> JG WHEN LAST PAY DATE IS PAST
      * JG -> EH WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E206 REJECT CD REQUIRED
      * E111 PRINCIPAL AMT NEGATIVE
      * E092 PRINCIPAL AMT OVER 1000000
      * E582 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E206' TO WS-REJECT-CODE
               WHEN TRST-PRINCIPAL-AMT < 0
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN TRST-PRINCIPAL-AMT > 1000000.00
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRST-TRUSTEE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRST-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TRST-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRSM-PRINCIPAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-PRINCIPAL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TRSM-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E615 REJECT CD REQUIRED
      * E035 TRUSTEE FEE AMT NEGATIVE
      * E208 TRUSTEE FEE AMT OVER 50000
      * E852 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT < 0
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT > 50000.00
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3680-LIMIT-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT < 100.00
               MOVE 100.00 TO TRSM-TRUSTEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TRUSTEE-FEE-AMT > 500.00
               MOVE 500.00 TO TRSM-TRUSTEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E590 ADDR REQUIRED
      * E923 TAX AMT NEGATIVE
      * E259 TAX AMT OVER 1000000
      * E904 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-ADDR = SPACES
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN TRSM-TAX-AMT < 0
                   MOVE 'E923' TO WS-REJECT-CODE
               WHEN TRSM-TAX-AMT > 1000000.00
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN TRSM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INCOME AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-INCOME.
           IF TRST-INCOME-AMT < 100.00
               MOVE 100.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-INCOME-AMT > 10000.00
               MOVE 10000.00 TO TRST-INCOME-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-YTD - (WS-WORK-AMT * 2)
           COMPUTE TRSM-TRUSTEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRSM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E730 DESC REQUIRED
      * E285 TAX YTD NEGATIVE
      * E594 TAX YTD OVER 1000000
      * E152 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD < 0
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN TRSM-TAX-YTD > 1000000.00
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN TRSM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REASON-CD
                   ADD TRSM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRST-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E552 MEMO REQUIRED
      * E926 INCOME AMT NEGATIVE
      * E090 INCOME AMT OVER 5000
      * E019 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E552' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-AMT < 0
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN TRSM-INCOME-AMT > 5000.00
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN TRSM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE INCOME AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3790-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRST-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRST-INCOME-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 7.00 TO TRST-DISTRIB-AMT
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 7.00 TO TRST-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE TRST-DISTRIB-AMT ROUNDED = TRST-INCOME-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRST-DISTRIB-AMT > 0 AND TRST-DISTRIB-AMT < 5.00
               MOVE 5.00 TO TRST-DISTRIB-AMT
           END-IF
           IF TRST-DISTRIB-AMT > 150.00
               MOVE 150.00 TO TRST-DISTRIB-AMT
           END-IF
           ADD TRST-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN EFF DATE IS PAST
      * D -> N WHEN EFF DATE IS PAST
      * N -> T WHEN EFF DATE IS PAST
      * T -> A WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRST-EFF-DATE > 0 AND TRST-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-DISTRIB.
           IF TRST-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-DISTRIB-AMT * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRSM-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE TAX AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3820-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRSM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRSM-TAX-AMT *
                       TRSM-RATE / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRSM-TAX-AMT *
                       TRSM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE TRSM-INCOME-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT < 100.00
               MOVE 100.00 TO TRSM-TRUSTEE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TRUSTEE-FEE-AMT > 10000.00
               MOVE 10000.00 TO TRSM-TRUSTEE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST FREQ CD:
      * E -> K WHEN LAST STMT DATE IS PAST
      * K -> P WHEN LAST STMT DATE IS PAST
      * P -> W WHEN LAST STMT DATE IS PAST
      * W -> Y WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INCOME AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-INCOME-AMT ROUNDED =
                   TRST-DISTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-INCOME-AMT
           END-IF
           ADD TRSM-INCOME-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE INCOME AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRST-INCOME-AMT NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN LAST PAY DATE IS PAST
      * MNG -> VFN WHEN LAST PAY DATE IS PAST
      * VFN -> XPL WHEN LAST PAY DATE IS PAST
      * XPL -> FLJ WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARKET AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-MARKET.
           IF TRSM-MARKET-AMT < 0.00
               MOVE 0.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-MARKET-AMT > 50000.00
               MOVE 50000.00 TO TRSM-MARKET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-PAYMENT.
           IF TRSM-PAYMENT-CNT < 9999999
               ADD 1 TO TRSM-PAYMENT-CNT
           END-IF
           IF TRSM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3950-FLAG-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRST-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3960-DESCRIBE-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN CLOSE DATE IS PAST
      * MNG -> VFN WHEN CLOSE DATE IS PAST
      * VFN -> XPL WHEN CLOSE DATE IS PAST
      * XPL -> FLJ WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 12
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-INCOME.
           IF TRSM-INCOME-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-YTD * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRSM-TRUSTEE-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-TRUSTEE-FEE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = TRSM-TAX-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-TAX-YTD - (WS-WORK-AMT * 2)
           COMPUTE TRSM-DISTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4050-ACCRUE-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-YTD * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-MARKET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4060-FLAG-MARKET.
           IF TRSM-MARKET-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-MARKET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARKET AMT.
      *----------------------------------------------------------------*
       4070-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-AMT ROUNDED =
                   TRSM-DISTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-MARKET-AMT
           END-IF
           ADD TRSM-MARKET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INCOME BAND TIER FOR THE MARKET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INCOME-BAND-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRSM-MARKET-AMT NOT < WS-INCOME-BAND-LOW(WS-TX)
                   MOVE WS-INCOME-BAND-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARKET YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4090-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-MARKET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-MARKET-YTD ROUNDED = TRSM-PRINCIPAL-AMT
                       * TRSM-RATE / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO TRSM-MARKET-YTD
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO TRSM-MARKET-YTD
               WHEN OTHER
                   COMPUTE TRSM-MARKET-YTD ROUNDED = TRSM-PRINCIPAL-AMT
                       * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-MARKET-YTD > 0 AND TRSM-MARKET-YTD < 10.00
               MOVE 10.00 TO TRSM-MARKET-YTD
           END-IF
           IF TRSM-MARKET-YTD > 250.00
               MOVE 250.00 TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BAND TIER FOR THE INCOME AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-INCOME-AMT NOT < WS-PRINCIPA-BAND-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-REGION-CD
                   ADD TRSM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4120-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   COMPUTE TRSM-TAX-YTD ROUNDED = WS-WORK-AMT-2 *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 15.00 TO TRSM-TAX-YTD
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 15.00 TO TRSM-TAX-YTD
               WHEN OTHER
                   COMPUTE TRSM-TAX-YTD ROUNDED = WS-WORK-AMT-2 *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-TAX-YTD > 0 AND TRSM-TAX-YTD < 10.00
               MOVE 10.00 TO TRSM-TAX-YTD
           END-IF
           IF TRSM-TAX-YTD > 35.00
               MOVE 35.00 TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> DS WHEN EFFECTIVE DATE IS PAST
      * DS -> FE WHEN EFFECTIVE DATE IS PAST
      * FE -> RB WHEN EFFECTIVE DATE IS PAST
      * RB -> TX WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF TRSM-EFFECTIVE-DATE > 0 AND TRSM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4180-LIMIT-TAX.
           IF TRSM-TAX-AMT < 0.00
               MOVE 0.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-AMT > 5000.00
               MOVE 5000.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4190-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E684 DESC REQUIRED
      * E511 TRUSTEE FEE YTD NEGATIVE
      * E894 TRUSTEE FEE YTD OVER 10000
      * E644 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD < 0
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-YTD > 10000.00
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRUSTEE FEE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4210-ACCRUE-TRUSTEE-FEE.
           IF TRST-TRUSTEE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-TRUSTEE-FEE-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0 AND TRST-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA BAND TIER FOR THE MARKET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF TRSM-MARKET-YTD NOT < WS-PRINCIPA-BAND-LOW(WS-TX)
                   MOVE WS-PRINCIPA-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TRPEN.
           INITIALIZE TRPEN-PARMS
           MOVE TRSM-DISTRIB-YTD
             TO TRPEN-DISTRIB-YTD
           MOVE TRSM-TRUSTEE-FEE-AMT
             TO TRPEN-TRUSTEE-FEE-AMT
           MOVE TRSM-INCOME-AMT
             TO TRPEN-INCOME-AMT
           MOVE TRSM-INCOME-YTD
             TO TRPEN-INCOME-YTD
           MOVE TRSM-TAX-AMT
             TO TRPEN-TAX-AMT
           MOVE TRSM-MARKET-YTD
             TO TRPEN-MARKET-YTD
           MOVE TRSM-CLOSE-DATE
             TO TRPEN-CLOSE-DATE
           MOVE TRSM-NOTICE-DATE
             TO TRPEN-NOTICE-DATE
           MOVE TRSM-LAST-STMT-DATE
             TO TRPEN-LAST-STMT-DATE
           MOVE TRSM-OPEN-DATE
             TO TRPEN-OPEN-DATE
           MOVE TRSM-REGION-CD
             TO TRPEN-REGION-CD
           MOVE TRSM-CYCLE-CD
             TO TRPEN-CYCLE-CD
           MOVE TRSM-STATUS-CD
             TO TRPEN-STATUS-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRPEN-TRUST-TYPE-CD
           MOVE TRSM-SUSPEND-FLAG
             TO TRPEN-SUSPEND-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRPEN-EXEMPT-FLAG
           MOVE TRSM-PREFERRED-FLAG
             TO TRPEN-PREFERRED-FLAG
           MOVE TRSM-RETRY-CNT
             TO TRPEN-RETRY-CNT
           MOVE TRSM-PAYMENT-CNT
             TO TRPEN-PAYMENT-CNT
           MOVE TRSM-DISCOUNT
             TO TRPEN-DISCOUNT
           MOVE TRSM-RATE
             TO TRPEN-RATE
           MOVE TRSM-ADDR
             TO TRPEN-ADDR
           MOVE TRSM-REF-NO
             TO TRPEN-REF-NO
           CALL 'TRPEN' USING TRPEN-PARMS
           EVALUATE TRPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRPEN-TRUSTEE-FEE-AMT
                     TO TRSM-TRUSTEE-FEE-AMT
                   MOVE TRPEN-INCOME-AMT
                     TO TRSM-INCOME-AMT
                   MOVE TRPEN-MARKET-YTD
                     TO TRSM-MARKET-YTD
                   MOVE TRPEN-SUSPEND-FLAG
                     TO TRSM-SUSPEND-FLAG
                   MOVE TRPEN-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TRPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TRLIM.
           INITIALIZE TRLIM-PARMS
           MOVE TRSM-TRUSTEE-FEE-AMT
             TO TRLIM-TRUSTEE-FEE-AMT
           MOVE TRSM-DISTRIB-YTD
             TO TRLIM-DISTRIB-YTD
           MOVE TRSM-DISTRIB-AMT
             TO TRLIM-DISTRIB-AMT
           MOVE TRSM-INCOME-YTD
             TO TRLIM-INCOME-YTD
           MOVE TRSM-INCOME-AMT
             TO TRLIM-INCOME-AMT
           MOVE TRSM-PRINCIPAL-YTD
             TO TRLIM-PRINCIPAL-YTD
           MOVE TRSM-CLOSE-DATE
             TO TRLIM-CLOSE-DATE
           MOVE TRSM-LAST-MAINT-DATE
             TO TRLIM-LAST-MAINT-DATE
           MOVE TRSM-OPEN-DATE
             TO TRLIM-OPEN-DATE
           MOVE TRSM-NOTICE-DATE
             TO TRLIM-NOTICE-DATE
           MOVE TRSM-CYCLE-CD
             TO TRLIM-CYCLE-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRLIM-TRUST-TYPE-CD
           MOVE TRSM-REASON-CD
             TO TRLIM-REASON-CD
           MOVE TRSM-STATUS-CD
             TO TRLIM-STATUS-CD
           MOVE TRSM-STOP-FLAG
             TO TRLIM-STOP-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRLIM-EXEMPT-FLAG
           MOVE TRSM-SUSPEND-FLAG
             TO TRLIM-SUSPEND-FLAG
           MOVE TRSM-PAYMENT-CNT
             TO TRLIM-PAYMENT-CNT
           MOVE TRSM-RETRY-CNT
             TO TRLIM-RETRY-CNT
           MOVE TRSM-MARGIN
             TO TRLIM-MARGIN
           MOVE TRSM-RATE
             TO TRLIM-RATE
           MOVE TRSM-NAME
             TO TRLIM-NAME
           MOVE TRSM-CUST-ID
             TO TRLIM-CUST-ID
           CALL 'TRLIM' USING TRLIM-PARMS
           EVALUATE TRLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRLIM-TRUSTEE-FEE-AMT
                     TO TRSM-TRUSTEE-FEE-AMT
                   MOVE TRLIM-DISTRIB-YTD
                     TO TRSM-DISTRIB-YTD
                   MOVE TRLIM-DISTRIB-AMT
                     TO TRSM-DISTRIB-AMT
                   MOVE TRLIM-INCOME-YTD
                     TO TRSM-INCOME-YTD
                   MOVE TRLIM-INCOME-AMT
                     TO TRSM-INCOME-AMT
                   MOVE TRLIM-PRINCIPAL-YTD
                     TO TRSM-PRINCIPAL-YTD
                   MOVE TRLIM-STOP-FLAG
                     TO TRSM-STOP-FLAG
                   MOVE TRLIM-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   MOVE TRLIM-SUSPEND-FLAG
                     TO TRSM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TRLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TRVAL.
           INITIALIZE TRVAL-PARMS
           MOVE TRSM-MARKET-AMT
             TO TRVAL-MARKET-AMT
           MOVE TRSM-TAX-AMT
             TO TRVAL-TAX-AMT
           MOVE TRSM-PRINCIPAL-YTD
             TO TRVAL-PRINCIPAL-YTD
           MOVE TRSM-DISTRIB-YTD
             TO TRVAL-DISTRIB-YTD
           MOVE TRSM-MARKET-YTD
             TO TRVAL-MARKET-YTD
           MOVE TRSM-PRINCIPAL-AMT
             TO TRVAL-PRINCIPAL-AMT
           MOVE TRSM-NOTICE-DATE
             TO TRVAL-NOTICE-DATE
           MOVE TRSM-LAST-MAINT-DATE
             TO TRVAL-LAST-MAINT-DATE
           MOVE TRSM-MATURITY-DATE
             TO TRVAL-MATURITY-DATE
           MOVE TRSM-CLOSE-DATE
             TO TRVAL-CLOSE-DATE
           MOVE TRSM-REASON-CD
             TO TRVAL-REASON-CD
           MOVE TRSM-REGION-CD
             TO TRVAL-REGION-CD
           MOVE TRSM-STATUS-CD
             TO TRVAL-STATUS-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRVAL-TRUST-TYPE-CD
           MOVE TRSM-NOTICE-SENT-FLAG
             TO TRVAL-NOTICE-SENT-FLAG
           MOVE TRSM-REVIEW-FLAG
             TO TRVAL-REVIEW-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRVAL-EXEMPT-FLAG
           MOVE TRSM-CYCLE-CNT
             TO TRVAL-CYCLE-CNT
           MOVE TRSM-NOTICE-CNT
             TO TRVAL-NOTICE-CNT
           MOVE TRSM-RATE
             TO TRVAL-RATE
           MOVE TRSM-MARGIN
             TO TRVAL-MARGIN
           MOVE TRSM-ADDR
             TO TRVAL-ADDR
           MOVE TRSM-CONTACT
             TO TRVAL-CONTACT
           CALL 'TRVAL' USING TRVAL-PARMS
           EVALUATE TRVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRVAL-PRINCIPAL-YTD
                     TO TRSM-PRINCIPAL-YTD
                   MOVE TRVAL-NOTICE-SENT-FLAG
                     TO TRSM-NOTICE-SENT-FLAG
                   MOVE TRVAL-REVIEW-FLAG
                     TO TRSM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TRVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TRST-KEY TO RD-KEY
           MOVE TRST-TRAN-CD TO RD-CODE
           MOVE TRST-INCOME-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TRST-KEY TO RD-KEY
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
           DISPLAY 'TRSAGE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TRSAGE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
