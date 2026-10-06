      *================================================================*
      * PROGRAM  : FNNAUDT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : FOREIGN BANKNOTES - AUDIT TRAIL. READS THE FXN      *
      *            TRANSACTIONS, UPDATES THE FXN MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FOREIGN CURRENCY BANKNOTE       *
      *            ORDERS, BUYBACK AND MARGIN                          *
      * INPUT    : FXNMST (KSDS), FXNTRN, BANKCTL                      *
      * OUTPUT   : FXNOUT, FNNAURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, FNELG, FNVAL            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-12 EWS  ORIGINAL                                       *
      * 1996-05-28 EWS  MARGIN RERUN PROTECTION PRB3021                *
      * 1998-01-26 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-02-02 LMB  BUYBACK NOTICE TEXT PER LEGAL INC9457          *
      * 2004-12-15 PQR  BUYBACK TABLE EXPANDED REQ1799                 *
      * 2004-11-05 BHS  SHIPPING CALCULATION CORRECTED INC0257         *
      * 2005-08-15 RJT  AUDIT FINDING - BUYBACK CONTROL TOTALS REQ7173 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNNAUDT.
       AUTHOR. C R PATEL.

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
           SELECT REPORT-FILE ASSIGN TO "FNNAURPT"
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
                                           VALUE 19.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
      * WS-BUYBACK-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000802500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000726500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000645000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000040500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000052000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000375500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006850000000155500'.
       01  WS-BUYBACK-BAND-TBL REDEFINES WS-BUYBACK-BAND-VALUES.
           05  WS-BUYBACK-BAND-ROW OCCURS 7 TIMES.
               10  WS-BUYBACK-BAND-LOW     PIC 9(09)V99.
               10  WS-BUYBACK-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MARGIN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MARGIN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000844000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000448000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000611000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000256000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000143000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011850000000461000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000359000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000386500'.
       01  WS-MARGIN-TIER-TBL REDEFINES WS-MARGIN-TIER-VALUES.
           05  WS-MARGIN-TIER-ROW OCCURS 8 TIMES.
               10  WS-MARGIN-TIER-LOW      PIC 9(09)V99.
               10  WS-MARGIN-TIER-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  FNNAUDT  FOREIGN BANKNOTES'.
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
       COPY FNVALP.

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
               PERFORM 3000-PRORATE-SHIPPING
               PERFORM 3010-SELECT-RATE
               PERFORM 3020-EDIT-ADDR
               PERFORM 3030-COUNT-SEQ
               PERFORM 3040-ACCRUE-BUYBACK
               PERFORM 3050-LIMIT-BUYBACK
               PERFORM 3060-ACCRUE-MARGIN
               PERFORM 3070-ACCUM-CHANNEL
               PERFORM 3080-SELECT-MARGIN
               PERFORM 3090-EDIT-ADDR
               PERFORM 3100-DESCRIBE-TYPE
               PERFORM 3110-SET-CHANNEL
               PERFORM 3120-CHECK-EFF
               PERFORM 3130-EDIT-CUST-ID
               PERFORM 3140-PRORATE-FOREIGN
               PERFORM 3150-PRORATE-USD
               PERFORM 3160-ASSESS-PREFERRED
               PERFORM 3170-SPLIT-FOREIGN
               PERFORM 3180-COUNT-PAYMENT
               PERFORM 3190-ACCUM-TRAN
               PERFORM 3200-ACCRUE-FOREIGN
               PERFORM 3210-COUNT-PAYMENT
               PERFORM 3220-SELECT-PENALTY-PCT
               PERFORM 3230-EDIT-CUST-ID
               PERFORM 3240-COUNT-PAYMENT
               PERFORM 3250-ASSESS-EXEMPT
               PERFORM 3260-COUNT-NOTICE
               PERFORM 3270-FLAG-MARGIN
               PERFORM 3280-ASSESS-PREFERRED
               PERFORM 3290-ACCUM-CURRENCY
               PERFORM 3300-ACCUM-SOURCE
               PERFORM 3310-SPLIT-MARGIN
               PERFORM 3320-SELECT-PENALTY-PCT
               PERFORM 3330-EDIT-ADDR
               PERFORM 3340-CHECK-CLOSE
               PERFORM 3350-ASSESS-AUTO-RENEW
               PERFORM 3360-ACCUM-CURRENCY
               PERFORM 3370-AGE-EXPIRY
               PERFORM 3380-LIMIT-USD
               PERFORM 3390-SPLIT-MARGIN
               PERFORM 3400-FLAG-ITEM
               PERFORM 3410-ACCRUE-SHIPPING
               PERFORM 3420-ASSESS-PREFERRED
               PERFORM 3430-SPLIT-BUYBACK
               PERFORM 3440-COUNT-NOTICE
               PERFORM 3450-COUNT-PAYMENT
               PERFORM 3460-ACCRUE-SHIPPING
               PERFORM 3470-PRORATE-USD
               PERFORM 3480-LIMIT-FOREIGN
               PERFORM 3490-SET-ORDER-TYPE
               PERFORM 3500-SPLIT-MARGIN
               PERFORM 3510-SELECT-PENALTY-PCT
               PERFORM 3520-LIMIT-BUYBACK
               PERFORM 3530-LIMIT-FOREIGN
               PERFORM 3540-ACCRUE-FOREIGN
               PERFORM 3550-PRORATE-SHIPPING
               PERFORM 3560-DESCRIBE-BRANCH
               PERFORM 3570-LIMIT-FOREIGN
               PERFORM 3580-CHECK-LAST-PAY
               PERFORM 3590-COUNT-EXCEPT
               PERFORM 3600-SELECT-RATE
               PERFORM 3610-EDIT-REJECT
               PERFORM 3620-ASSESS-PREFERRED
               PERFORM 3630-ACCRUE-BUYBACK
               PERFORM 3640-SPLIT-BUYBACK
               PERFORM 3650-DESCRIBE-TRAN
               PERFORM 3660-CHECK-OPEN
               PERFORM 3670-ASSESS-EXEMPT
               PERFORM 3680-FLAG-USD
               PERFORM 3690-PRORATE-MARGIN
               PERFORM 3700-EDIT-DESC
               PERFORM 3710-CHECK-EFF
               PERFORM 3720-SELECT-PENALTY-PCT
               PERFORM 3730-CHECK-EFF
               PERFORM 3740-ACCRUE-MARGIN
               PERFORM 3750-COUNT-EXCEPT
               PERFORM 3760-SELECT-PENALTY-PCT
               PERFORM 3770-ACCUM-BRANCH
               PERFORM 3780-CHECK-MATURITY
               PERFORM 3790-SET-SOURCE
               PERFORM 3800-DESCRIBE-TYPE
               PERFORM 3810-DESCRIBE-BRANCH
               PERFORM 3820-LIMIT-BUYBACK
               PERFORM 3830-PRORATE-FOREIGN
               PERFORM 3840-FLAG-FOREIGN
               PERFORM 3850-ACCRUE-FOREIGN
               PERFORM 3860-PRORATE-USD
               PERFORM 3870-ASSESS-NOTICE-SENT
               PERFORM 3880-COUNT-SEQ
               PERFORM 3890-FLAG-BUYBACK
               PERFORM 3900-DESCRIBE-SOURCE
               PERFORM 3910-SET-CURRENCY
               PERFORM 3920-SPLIT-BUYBACK
               PERFORM 3930-DESCRIBE-BRANCH
               PERFORM 3940-ACCUM-CURRENCY
               PERFORM 3950-AGE-POST
               PERFORM 3960-CHECK-LAST-PAY
               PERFORM 3970-CHECK-CLOSE
               PERFORM 3980-PRORATE-USD
               PERFORM 3990-ACCRUE-MARGIN
               PERFORM 4000-CHECK-NOTICE
               PERFORM 4010-PRORATE-SHIPPING
               PERFORM 4020-CHECK-LAST-ACT
               PERFORM 4030-DESCRIBE-CURRENCY
               PERFORM 4040-COUNT-NOTICE
               PERFORM 4050-ASSESS-REVIEW
               PERFORM 4060-FLAG-MARGIN
               PERFORM 4070-ACCUM-BRANCH
               PERFORM 4080-LIMIT-BUYBACK
               PERFORM 4090-LIMIT-SHIPPING
               PERFORM 4100-SELECT-PENALTY-PCT
               PERFORM 4110-COUNT-EXCEPT
               PERFORM 4120-SET-BRANCH
               PERFORM 4130-PRORATE-MARGIN
               PERFORM 4140-PRORATE-USD
               PERFORM 4150-ACCRUE-MARGIN
               PERFORM 4160-CHECK-LAST-PAY
               PERFORM 4170-AGE-OPEN
               PERFORM 4180-SELECT-MARGIN
               PERFORM 4190-LIMIT-USD
               PERFORM 4200-ACCUM-ORDER-TYPE
               PERFORM 4210-EDIT-CUST-ID
               PERFORM 4220-AGE-LAST-MAINT
               PERFORM 4230-CHECK-MATURITY
               PERFORM 4240-ASSESS-OVERRIDE
               PERFORM 7010-CALL-FNELG
               PERFORM 7030-CALL-FNVAL
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
      * PRORATE SHIPPING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       3000-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   FXNM-SHIPPING-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNM-MARGIN-AMT NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E891 ADDR REQUIRED
      * E841 FOREIGN YTD NEGATIVE
      * E498 FOREIGN YTD OVER 10000
      * E412 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-ADDR = SPACES
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD < 0
                   MOVE 'E841' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD > 10000.00
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN FXNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-SEQ.
           IF FXNT-SEQ < 9999999
               ADD 1 TO FXNT-SEQ
           END-IF
           IF FXNT-SEQ NOT < 5
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNT-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 50.00
               MOVE 50.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 1000.00
               MOVE 1000.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-MARGIN.
           IF FXNM-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-MARGIN-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-SHIPPING-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNM-BUYBACK-AMT NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E432 ADDR REQUIRED
      * E971 BUYBACK YTD NEGATIVE
      * E129 BUYBACK YTD OVER 25000
      * E374 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-ADDR = SPACES
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD < 0
                   MOVE 'E971' TO WS-REJECT-CODE
               WHEN FXNM-BUYBACK-YTD > 25000.00
                   MOVE 'E129' TO WS-REJECT-CODE
               WHEN FXNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-CHANNEL.
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
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-EFF.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E490 CUST ID REQUIRED
      * E218 FOREIGN AMT NEGATIVE
      * E945 FOREIGN AMT OVER 1000000
      * E967 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E490' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT < 0
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT > 1000000.00
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E967' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE USD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-AMT ROUNDED =
                   FXNM-USD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3160-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = WS-WORK-AMT-2 *
                       FXNM-MARGIN / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 12.00 TO FXNM-SHIPPING-YTD
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 12.00 TO FXNM-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = WS-WORK-AMT-2 *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-SHIPPING-YTD > 0 AND FXNM-SHIPPING-YTD < 5.00
               MOVE 5.00 TO FXNM-SHIPPING-YTD
           END-IF
           IF FXNM-SHIPPING-YTD > 75.00
               MOVE 75.00 TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNM-FOREIGN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNT-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNM-MARGIN-AMT NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E421 CUST ID REQUIRED
      * E190 MARGIN AMT NEGATIVE
      * E780 MARGIN AMT OVER 1000000
      * E586 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT < 0
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-AMT > 1000000.00
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN FXNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO FXNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE USD YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3250-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-USD-YTD *
                       FXNM-RATE / 200
               WHEN FXNM-CURRENCY-CD = 'C'
                   MOVE 3.00 TO FXNM-SHIPPING-YTD
               WHEN FXNM-CURRENCY-CD = 'E'
                   MOVE 7.00 TO FXNM-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-USD-YTD *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-SHIPPING-YTD > 0 AND FXNM-SHIPPING-YTD < 5.00
               MOVE 5.00 TO FXNM-SHIPPING-YTD
           END-IF
           IF FXNM-SHIPPING-YTD > 35.00
               MOVE 35.00 TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-MARGIN.
           IF FXNT-MARGIN-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN FEE ON THE SHIPPING YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3280-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNT-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNM-SHIPPING-YTD
                       * FXNM-MARGIN / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 5.00 TO FXNT-FOREIGN-AMT
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 5.00 TO FXNT-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNM-SHIPPING-YTD
                       * FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-FOREIGN-AMT > 0 AND FXNT-FOREIGN-AMT < 2.00
               MOVE 2.00 TO FXNT-FOREIGN-AMT
           END-IF
           IF FXNT-FOREIGN-AMT > 50.00
               MOVE 50.00 TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK YTD BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-SOURCE.
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
      * SPLIT MARGIN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNM-SHIPPING-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN TIER TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNT-USD-AMT NOT < WS-MARGIN-TIER-LOW(WS-TX)
                   MOVE WS-MARGIN-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E066 ADDR REQUIRED
      * E089 FOREIGN YTD NEGATIVE
      * E324 FOREIGN YTD OVER 5000
      * E396 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-ADDR = SPACES
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD < 0
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-YTD > 5000.00
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN FXNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-CLOSE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE USD YTD FEE ON THE SHIPPING YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3350-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-USD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-USD-YTD ROUNDED = FXNM-SHIPPING-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 20.00 TO FXNM-USD-YTD
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 15.00 TO FXNM-USD-YTD
               WHEN OTHER
                   COMPUTE FXNM-USD-YTD ROUNDED = FXNM-SHIPPING-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-USD-YTD > 0 AND FXNM-USD-YTD < 2.00
               MOVE 2.00 TO FXNM-USD-YTD
           END-IF
           IF FXNM-USD-YTD > 75.00
               MOVE 75.00 TO FXNM-USD-YTD
           END-IF
           ADD FXNM-USD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EXPIRY.
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
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-USD.
           IF FXNT-USD-AMT < 25.00
               MOVE 25.00 TO FXNT-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-USD-AMT > 50000.00
               MOVE 50000.00 TO FXNT-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNT-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3400-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE FOREIGN YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3420-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-FOREIGN-YTD
                       * FXNM-RATE / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 20.00 TO FXNM-SHIPPING-YTD
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 3.00 TO FXNM-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-FOREIGN-YTD
                       * FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-SHIPPING-YTD > 0 AND FXNM-SHIPPING-YTD < 5.00
               MOVE 5.00 TO FXNM-SHIPPING-YTD
           END-IF
           IF FXNM-SHIPPING-YTD > 35.00
               MOVE 35.00 TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNM-BUYBACK-YTD / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-BUYBACK-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNM-MARGIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-USD-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FOREIGN YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-YTD < 100.00
               MOVE 100.00 TO FXNM-FOREIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-YTD > 10000.00
               MOVE 10000.00 TO FXNM-FOREIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN LAST MAINT DATE IS PAST
      * KZV -> PHV WHEN LAST MAINT DATE IS PAST
      * PHV -> PZZ WHEN LAST MAINT DATE IS PAST
      * PZZ -> WMC WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MARGIN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = FXNM-MARGIN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-MARGIN-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNT-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNM-MARGIN-AMT NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3520-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 10.00
               MOVE 10.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 500.00
               MOVE 500.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-AMT < 0.00
               MOVE 0.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-AMT > 50000.00
               MOVE 50000.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       3550-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   FXNM-MARGIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3560-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3570-LIMIT-FOREIGN.
           IF FXNT-FOREIGN-AMT < 0.00
               MOVE 0.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-FOREIGN-AMT > 10000.00
               MOVE 10000.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-LAST-PAY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN TIER TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-TIER-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNT-MARGIN-AMT NOT < WS-MARGIN-TIER-LOW(WS-TX)
                   MOVE WS-MARGIN-TIER-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E706 REJECT CD REQUIRED
      * E196 MARGIN YTD NEGATIVE
      * E031 MARGIN YTD OVER 10000
      * E740 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD < 0
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD > 10000.00
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN FXNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE USD FEE ON THE MARGIN AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3620-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNT-USD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-BRANCH-CD = 'APZ'
                   MOVE 5.00 TO FXNT-USD-AMT
               WHEN FXNM-BRANCH-CD = 'BBP'
                   MOVE 12.00 TO FXNT-USD-AMT
               WHEN OTHER
                   COMPUTE FXNT-USD-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNT-USD-AMT > 0 AND FXNT-USD-AMT < 1.00
               MOVE 1.00 TO FXNT-USD-AMT
           END-IF
           IF FXNT-USD-AMT > 150.00
               MOVE 150.00 TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNM-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNT-BUYBACK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-BUYBACK-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-MARGIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-OPEN.
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
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE USD AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3670-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = FXNT-USD-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 15.00 TO FXNM-BUYBACK-AMT
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 15.00 TO FXNM-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = FXNT-USD-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-BUYBACK-AMT > 0 AND FXNM-BUYBACK-AMT < 1.00
               MOVE 1.00 TO FXNM-BUYBACK-AMT
           END-IF
           IF FXNM-BUYBACK-AMT > 75.00
               MOVE 75.00 TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * USD AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3680-FLAG-USD.
           IF FXNT-USD-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3690-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E099 DESC REQUIRED
      * E655 FOREIGN AMT NEGATIVE
      * E101 FOREIGN AMT OVER 25000
      * E024 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT < 0
                   MOVE 'E655' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT > 25000.00
                   MOVE 'E101' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-EFF.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO FXNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN TIER TIER FOR THE MARGIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-MARGIN-AMT NOT < WS-MARGIN-TIER-LOW(WS-TX)
                   MOVE WS-MARGIN-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-EFF.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNT-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO FXNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNT-USD-AMT NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-MATURITY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN LAST ACT DATE IS PAST
      * FS -> HL WHEN LAST ACT DATE IS PAST
      * HL -> HS WHEN LAST ACT DATE IS PAST
      * HS -> NW WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-SOURCE.
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
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3820-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 100.00
               MOVE 100.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 50000.00
               MOVE 50000.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3830-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   FXNT-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3840-FLAG-FOREIGN.
           IF FXNT-FOREIGN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNT-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-FOREIGN.
           IF FXNT-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-MARGIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO USD YTD.
      *----------------------------------------------------------------*
       3860-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-USD-YTD
           END-IF
           ADD FXNM-USD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN YTD FEE ON THE FOREIGN AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3870-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-MARGIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   COMPUTE FXNM-MARGIN-YTD ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO FXNM-MARGIN-YTD
               WHEN FXNT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO FXNM-MARGIN-YTD
               WHEN OTHER
                   COMPUTE FXNM-MARGIN-YTD ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-MARGIN-YTD > 0 AND FXNM-MARGIN-YTD < 10.00
               MOVE 10.00 TO FXNM-MARGIN-YTD
           END-IF
           IF FXNM-MARGIN-YTD > 100.00
               MOVE 100.00 TO FXNM-MARGIN-YTD
           END-IF
           ADD FXNM-MARGIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-SEQ.
           IF FXNT-SEQ < 9999999
               ADD 1 TO FXNT-SEQ
           END-IF
           IF FXNT-SEQ NOT < 3
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3890-FLAG-BUYBACK.
           IF FXNT-BUYBACK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNT-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CURRENCY CD:
      * C -> E WHEN POST DATE IS PAST
      * E -> G WHEN POST DATE IS PAST
      * G -> P WHEN POST DATE IS PAST
      * P -> W WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO FXNM-CURRENCY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNT-BUYBACK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNT-BUYBACK-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNM-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN YTD BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD FXNM-MARGIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-MARGIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-POST.
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
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-LAST-PAY.
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
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-CLOSE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       3980-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-USD-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-MARGIN.
           IF FXNM-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-MARGIN-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNM-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-LAST-ACT.
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
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE FOREIGN AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4050-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO FXNT-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNT-FOREIGN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 10.00 TO FXNT-BUYBACK-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 20.00 TO FXNT-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNT-BUYBACK-AMT ROUNDED = FXNT-FOREIGN-AMT *
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
      * MARGIN YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4060-FLAG-MARGIN.
           IF FXNM-MARGIN-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-BRANCH-CD
                   ADD FXNT-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4080-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-AMT < 50.00
               MOVE 50.00 TO FXNM-BUYBACK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-AMT > 500.00
               MOVE 500.00 TO FXNM-BUYBACK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-SHIPPING.
           IF FXNM-SHIPPING-YTD < 50.00
               MOVE 50.00 TO FXNM-SHIPPING-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-SHIPPING-YTD > 50000.00
               MOVE 50000.00 TO FXNM-SHIPPING-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNM-FOREIGN-AMT NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO FXNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN EFF DATE IS PAST
      * BBP -> ENL WHEN EFF DATE IS PAST
      * ENL -> EZB WHEN EFF DATE IS PAST
      * EZB -> KHV WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARGIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       4130-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-MARGIN-AMT ROUNDED =
                   FXNM-MARGIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       4140-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-USD-AMT ROUNDED =
                   FXNM-MARGIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-USD-AMT
           END-IF
           ADD FXNT-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-SHIPPING-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-LAST-PAY.
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
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-OPEN.
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
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MARGIN TIER TIER FOR THE FOREIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4180-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MARGIN-TIER-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNT-FOREIGN-AMT NOT < WS-MARGIN-TIER-LOW(WS-TX)
                   MOVE WS-MARGIN-TIER-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4190-LIMIT-USD.
           IF FXNM-USD-AMT < 10.00
               MOVE 10.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 2500.00
               MOVE 2500.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY ORDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E637 CUST ID REQUIRED
      * E412 USD AMT NEGATIVE
      * E405 USD AMT OVER 1000000
      * E792 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN FXNT-USD-AMT < 0
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN FXNT-USD-AMT > 1000000.00
                   MOVE 'E405' TO WS-REJECT-CODE
               WHEN FXNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-LAST-MAINT.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-MATURITY.
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
      * ASSESS THE FOREIGN FEE ON THE FOREIGN YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4240-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNT-FOREIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-MARGIN / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 7.00 TO FXNT-FOREIGN-AMT
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 20.00 TO FXNT-FOREIGN-AMT
               WHEN OTHER
                   COMPUTE FXNT-FOREIGN-AMT ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNT-FOREIGN-AMT > 0 AND FXNT-FOREIGN-AMT < 5.00
               MOVE 5.00 TO FXNT-FOREIGN-AMT
           END-IF
           IF FXNT-FOREIGN-AMT > 150.00
               MOVE 150.00 TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-FEES.

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
                   MOVE FNELG-BUYBACK-AMT
                     TO FXNM-BUYBACK-AMT
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
      * PASS THE FXN FIELDS TO FNVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-FNVAL.
           INITIALIZE FNVAL-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNVAL-FOREIGN-AMT
           MOVE FXNM-SHIPPING-AMT
             TO FNVAL-SHIPPING-AMT
           MOVE FXNM-FOREIGN-YTD
             TO FNVAL-FOREIGN-YTD
           MOVE FXNM-SHIPPING-YTD
             TO FNVAL-SHIPPING-YTD
           MOVE FXNM-MARGIN-AMT
             TO FNVAL-MARGIN-AMT
           MOVE FXNM-USD-AMT
             TO FNVAL-USD-AMT
           MOVE FXNM-OPEN-DATE
             TO FNVAL-OPEN-DATE
           MOVE FXNM-EXPIRY-DATE
             TO FNVAL-EXPIRY-DATE
           MOVE FXNM-NOTICE-DATE
             TO FNVAL-NOTICE-DATE
           MOVE FXNM-LAST-ACT-DATE
             TO FNVAL-LAST-ACT-DATE
           MOVE FXNM-TYPE-CD
             TO FNVAL-TYPE-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNVAL-ORDER-TYPE-CD
           MOVE FXNM-SOURCE-CD
             TO FNVAL-SOURCE-CD
           MOVE FXNM-BRANCH-CD
             TO FNVAL-BRANCH-CD
           MOVE FXNM-EXEMPT-FLAG
             TO FNVAL-EXEMPT-FLAG
           MOVE FXNM-NOTICE-SENT-FLAG
             TO FNVAL-NOTICE-SENT-FLAG
           MOVE FXNM-REVIEW-FLAG
             TO FNVAL-REVIEW-FLAG
           MOVE FXNM-EXCEPT-CNT
             TO FNVAL-EXCEPT-CNT
           MOVE FXNM-PAYMENT-CNT
             TO FNVAL-PAYMENT-CNT
           MOVE FXNM-PENALTY-PCT
             TO FNVAL-PENALTY-PCT
           MOVE FXNM-RATE
             TO FNVAL-RATE
           MOVE FXNM-CONTACT
             TO FNVAL-CONTACT
           MOVE FXNM-REF-NO
             TO FNVAL-REF-NO
           CALL 'FNVAL' USING FNVAL-PARMS
           EVALUATE FNVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNVAL-MARGIN-AMT
                     TO FXNM-MARGIN-AMT
                   MOVE FNVAL-NOTICE-SENT-FLAG
                     TO FXNM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-FNVAL' TO ABND-PARAGRAPH
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
           DISPLAY 'FNNAUDT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'FNNAUDT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
