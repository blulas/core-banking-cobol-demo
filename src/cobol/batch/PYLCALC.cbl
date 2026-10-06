      *================================================================*
      * PROGRAM  : PYLCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PAYROLL SERVICES - PERIODIC CALCULATION. READS THE  *
      *            PRL TRANSACTIONS, UPDATES THE PRL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. CLIENT PAYROLL         *
      *            CALCULATION, TAX DEPOSIT AND DIRECT DEPOSIT         *
      * INPUT    : PRLMST (KSDS), PRLTRN, BANKCTL                      *
      * OUTPUT   : PRLOUT, PYLCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PYFEE, PYTIER           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-05-04 RJT  ORIGINAL                                       *
      * 1989-11-06 JLM  DEDUCT CUTOFF MOVED INC9589                    *
      * 1989-05-17 DPS  DEDUCT CALCULATION CORRECTED REQ6054           *
      * 1995-08-11 BHS  GROSS CUTOFF MOVED REQ1477                     *
      * 1996-05-04 TWH  NET FEE SCHEDULE UPDATE PRB5232                *
      * 1997-10-14 RJT  FED-TAX RERUN PROTECTION REQ3017               *
      * 2004-08-11 TWH  DEDUCT RERUN PROTECTION CHG8636                *
      * 2004-02-13 TWH  NET TABLE EXPANDED CHG0161                     *
      * 2005-08-24 MKL  FICA FEE SCHEDULE UPDATE CHG8771               *
      * 2008-06-19 JLM  NET EXCEPTIONS TO OPS QUEUE PRB6387            *
      * 2011-10-04 BHS  PERFORMANCE - GROSS READ AHEAD CHG0791         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYLCALC.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PRLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PRLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PRLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PRLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PYLCARPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PRLMST.
       FD  TRAN-FILE.
       COPY PRLTRN.
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
                                           VALUE 22.000000.
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
      * WS-FICA-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FICA-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000542000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000597000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000495500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000060000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000150500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018000000000220000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028000000000825000'.
       01  WS-FICA-BAND-TBL REDEFINES WS-FICA-BAND-VALUES.
           05  WS-FICA-BAND-ROW OCCURS 7 TIMES.
               10  WS-FICA-BAND-LOW        PIC 9(09)V99.
               10  WS-FICA-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GROSS-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GROSS-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000567500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000830500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000358000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000876000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000487000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000235000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000132500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023100000000368000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023600000000876000'.
       01  WS-GROSS-BAND-TBL REDEFINES WS-GROSS-BAND-VALUES.
           05  WS-GROSS-BAND-ROW OCCURS 9 TIMES.
               10  WS-GROSS-BAND-LOW       PIC 9(09)V99.
               10  WS-GROSS-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STATE-TA-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STATE-TA-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000724000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000818000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000691000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000806000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000643000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000748500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000827000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016550000000840000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019050000000622500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019100000000616000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021600000000069000'.
       01  WS-STATE-TA-BAND-TBL REDEFINES WS-STATE-TA-BAND-VALUES.
           05  WS-STATE-TA-BAND-ROW OCCURS 11 TIMES.
               10  WS-STATE-TA-BAND-LOW    PIC 9(09)V99.
               10  WS-STATE-TA-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FICA-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FICA-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000237500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000159000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000592500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000405500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000086500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000688500'.
       01  WS-FICA-TIER-TBL REDEFINES WS-FICA-TIER-VALUES.
           05  WS-FICA-TIER-ROW OCCURS 6 TIMES.
               10  WS-FICA-TIER-LOW        PIC 9(09)V99.
               10  WS-FICA-TIER-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PYLCALC  PAYROLL SERVICES'.
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
       COPY PYFEEP.
       COPY PYTIERP.

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
               MOVE 'PRLMST' TO ABND-FILE-NAME
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
           MOVE PRLT-KEY TO PRLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SELECT-MARGIN
               PERFORM 3010-ACCUM-REASON
               PERFORM 3020-LIMIT-FED-TAX
               PERFORM 3030-SELECT-RATE
               PERFORM 3040-SET-PAY-FREQ
               PERFORM 3050-PRORATE-FICA
               PERFORM 3060-ASSESS-AUTO-RENEW
               PERFORM 3070-FLAG-FICA
               PERFORM 3080-EDIT-CONTACT
               PERFORM 3090-DESCRIBE-TRAN
               PERFORM 3100-CHECK-LAST-PAY
               PERFORM 3110-FLAG-DEDUCT
               PERFORM 3120-PRORATE-GROSS
               PERFORM 3130-LIMIT-STATE-TAX
               PERFORM 3140-AGE-LAST-ACT
               PERFORM 3150-DESCRIBE-REGION
               PERFORM 3160-ACCUM-TRAN
               PERFORM 3170-CHECK-EFFECTIVE
               PERFORM 3180-CHECK-POST
               PERFORM 3190-COUNT-PAYMENT
               PERFORM 3200-SELECT-FEE-PCT
               PERFORM 3210-ACCRUE-NET
               PERFORM 3220-SET-STATUS
               PERFORM 3230-AGE-CLOSE
               PERFORM 3240-SELECT-RATE
               PERFORM 3250-SET-PAY-FREQ
               PERFORM 3260-ACCRUE-STATE-TAX
               PERFORM 3270-EDIT-MEMO
               PERFORM 3280-ACCUM-STATUS
               PERFORM 3290-SELECT-MARGIN
               PERFORM 3300-ACCUM-REGION
               PERFORM 3310-SPLIT-DEDUCT
               PERFORM 3320-SET-PAY-FREQ
               PERFORM 3330-SELECT-RATE
               PERFORM 3340-AGE-POST
               PERFORM 3350-SPLIT-DEDUCT
               PERFORM 3360-EDIT-REJECT
               PERFORM 3370-DESCRIBE-PAY-FREQ
               PERFORM 3380-LIMIT-GROSS
               PERFORM 3390-ACCRUE-STATE-TAX
               PERFORM 3400-LIMIT-FICA
               PERFORM 3410-DESCRIBE-REASON
               PERFORM 3420-EDIT-OPERATOR
               PERFORM 3430-PRORATE-GROSS
               PERFORM 3440-ASSESS-NOTICE-SENT
               PERFORM 3450-EDIT-ADDR
               PERFORM 3460-EDIT-CONTACT
               PERFORM 3470-SELECT-MARGIN
               PERFORM 3480-AGE-NOTICE
               PERFORM 3490-AGE-NEXT-REVIEW
               PERFORM 3500-SPLIT-STATE-TAX
               PERFORM 3510-ASSESS-HOLD
               PERFORM 3520-AGE-CLOSE
               PERFORM 3530-SELECT-FEE-PCT
               PERFORM 3540-PRORATE-DEDUCT
               PERFORM 3550-COUNT-PAYMENT
               PERFORM 3560-FLAG-NET
               PERFORM 3570-FLAG-FED-TAX
               PERFORM 3580-AGE-CLOSE
               PERFORM 3590-LIMIT-DEDUCT
               PERFORM 3600-COUNT-NOTICE
               PERFORM 3610-COUNT-ITEM
               PERFORM 3620-FLAG-STATE-TAX
               PERFORM 3630-DESCRIBE-SOURCE
               PERFORM 3640-DESCRIBE-STATUS
               PERFORM 3650-EDIT-NAME
               PERFORM 3660-LIMIT-FED-TAX
               PERFORM 3670-AGE-NEXT-DUE
               PERFORM 3680-PRORATE-STATE-TAX
               PERFORM 3690-AGE-POST
               PERFORM 3700-ACCUM-SOURCE
               PERFORM 3710-EDIT-CUST-ID
               PERFORM 3720-CHECK-EXPIRY
               PERFORM 3730-CHECK-EFF
               PERFORM 3740-ASSESS-HOLD
               PERFORM 3750-ACCUM-TAX-STATE
               PERFORM 3760-SET-CHANNEL
               PERFORM 3770-COUNT-PAYMENT
               PERFORM 3780-PRORATE-GROSS
               PERFORM 3790-SET-SOURCE
               PERFORM 3800-AGE-CLOSE
               PERFORM 3810-ASSESS-SUSPEND
               PERFORM 3820-EDIT-REF-NO
               PERFORM 3830-EDIT-MEMO
               PERFORM 3840-SELECT-FEE-PCT
               PERFORM 3850-ACCUM-PAY-FREQ
               PERFORM 3860-SET-TAX-STATE
               PERFORM 3870-AGE-OPEN
               PERFORM 3880-ASSESS-STOP
               PERFORM 3890-SPLIT-DEDUCT
               PERFORM 3900-ACCUM-REGION
               PERFORM 3910-SELECT-FEE-PCT
               PERFORM 3920-EDIT-CONTACT
               PERFORM 3930-PRORATE-GROSS
               PERFORM 3940-AGE-EFF
               PERFORM 3950-ASSESS-REVERSAL
               PERFORM 3960-EDIT-CONTACT
               PERFORM 3970-FLAG-STATE-TAX
               PERFORM 3980-SPLIT-STATE-TAX
               PERFORM 3990-DESCRIBE-TAX-STATE
               PERFORM 4000-COUNT-SEQ
               PERFORM 4010-SPLIT-DEDUCT
               PERFORM 4020-SET-SOURCE
               PERFORM 4030-SPLIT-STATE-TAX
               PERFORM 4040-SPLIT-STATE-TAX
               PERFORM 4050-SELECT-RATE
               PERFORM 4060-PRORATE-DEDUCT
               PERFORM 4070-ACCUM-REASON
               PERFORM 4080-COUNT-ITEM
               PERFORM 4090-PRORATE-ITEM
               PERFORM 4100-PRORATE-NET
               PERFORM 4110-PRORATE-DEDUCT
               PERFORM 4120-FLAG-GROSS
               PERFORM 4130-AGE-EFF
               PERFORM 4140-SELECT-RATE
               PERFORM 7010-CALL-PYFEE
               PERFORM 7020-CALL-PYTIER
               REWRITE PRL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PRLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PRL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SELECT THE FICA TIER TIER FOR THE GROSS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-TIER-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PRLM-GROSS-YTD NOT < WS-FICA-TIER-LOW(WS-TX)
                   MOVE WS-FICA-TIER-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REASON-CD
                   ADD PRLM-GROSS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-GROSS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3020-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 10.00
               MOVE 10.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 50000.00
               MOVE 50000.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-BAND-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-FICA-BAND-LOW(WS-TX)
                   MOVE WS-FICA-BAND-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN EXPIRY DATE IS PAST
      * XM -> ZT WHEN EXPIRY DATE IS PAST
      * ZT -> JT WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FICA AMT.
      *----------------------------------------------------------------*
       3050-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-FICA-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FICA YTD FEE ON THE NET AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3060-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FICA-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLM-NET-AMT *
                       PRLM-MARGIN / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 12.00 TO PRLM-FICA-YTD
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 3.00 TO PRLM-FICA-YTD
               WHEN OTHER
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLM-NET-AMT *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-FICA-YTD > 0 AND PRLM-FICA-YTD < 1.00
               MOVE 1.00 TO PRLM-FICA-YTD
           END-IF
           IF PRLM-FICA-YTD > 100.00
               MOVE 100.00 TO PRLM-FICA-YTD
           END-IF
           ADD PRLM-FICA-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FICA AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3070-FLAG-FICA.
           IF PRLT-FICA-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLT-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E017 CONTACT REQUIRED
      * E373 FICA AMT NEGATIVE
      * E324 FICA AMT OVER 5000
      * E219 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CONTACT = SPACES
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT < 0
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT > 5000.00
                   MOVE 'E324' TO WS-REJECT-CODE
               WHEN PRLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E219' TO WS-REJECT-CODE
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
       3090-DESCRIBE-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'AD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3110-FLAG-DEDUCT.
           IF PRLM-DEDUCT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-DEDUCT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GROSS YTD.
      *----------------------------------------------------------------*
       3120-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-YTD ROUNDED =
                   PRLT-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-GROSS-YTD
           END-IF
           ADD PRLM-GROSS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATE TAX YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3130-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-YTD < 100.00
               MOVE 100.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-YTD > 2500.00
               MOVE 2500.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE NET YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLM-NET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PRLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-FED-TAX-YTD NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-NET.
           IF PRLM-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-AMT * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-FED-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN LAST PAY DATE IS PAST
      * RD -> ST WHEN LAST PAY DATE IS PAST
      * ST -> TJ WHEN LAST PAY DATE IS PAST
      * TJ -> UC WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA BAND TIER FOR THE DEDUCT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-BAND-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PRLM-DEDUCT-YTD NOT < WS-STATE-TA-BAND-LOW(WS-TX)
                   MOVE WS-STATE-TA-BAND-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN NOTICE DATE IS PAST
      * XM -> ZT WHEN NOTICE DATE IS PAST
      * ZT -> JT WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-STATE-TAX.
           IF PRLT-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-STATE-TAX-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E252 MEMO REQUIRED
      * E260 FICA YTD NEGATIVE
      * E881 FICA YTD OVER 10000
      * E490 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E252' TO WS-REJECT-CODE
               WHEN PRLM-FICA-YTD < 0
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN PRLM-FICA-YTD > 10000.00
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN PRLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E490' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-STATUS-CD
                   ADD PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-BAND-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-FICA-BAND-LOW(WS-TX)
                   MOVE WS-FICA-BAND-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN OPEN DATE IS PAST
      * XM -> ZT WHEN OPEN DATE IS PAST
      * ZT -> JT WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLT-STATE-TAX-AMT NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PRLM-GROSS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E213 REJECT CD REQUIRED
      * E868 GROSS AMT NEGATIVE
      * E721 GROSS AMT OVER 250000
      * E963 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-REJECT-CD = SPACES
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT < 0
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT > 250000.00
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN PRLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GROSS YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3380-LIMIT-GROSS.
           IF PRLM-GROSS-YTD < 0.00
               MOVE 0.00 TO PRLM-GROSS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-GROSS-YTD > 500.00
               MOVE 500.00 TO PRLM-GROSS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-GROSS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FICA YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3400-LIMIT-FICA.
           IF PRLM-FICA-YTD < 0.00
               MOVE 0.00 TO PRLM-FICA-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FICA-YTD > 50000.00
               MOVE 50000.00 TO PRLM-FICA-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E478 OPERATOR REQUIRED
      * E979 FICA AMT NEGATIVE
      * E096 FICA AMT OVER 250000
      * E660 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-OPERATOR = SPACES
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT < 0
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT > 250000.00
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GROSS AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-AMT ROUNDED =
                   PRLM-FICA-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3440-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = WS-WORK-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 5.00 TO PRLM-DEDUCT-YTD
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 15.00 TO PRLM-DEDUCT-YTD
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = WS-WORK-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-DEDUCT-YTD > 0 AND PRLM-DEDUCT-YTD < 2.00
               MOVE 2.00 TO PRLM-DEDUCT-YTD
           END-IF
           IF PRLM-DEDUCT-YTD > 150.00
               MOVE 150.00 TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E749 ADDR REQUIRED
      * E249 WORK AMT 2 NEGATIVE
      * E328 WORK AMT 2 OVER 1000000
      * E482 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E249' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN PRLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E870 CONTACT REQUIRED
      * E022 DEDUCT AMT NEGATIVE
      * E591 DEDUCT AMT OVER 25000
      * E885 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CONTACT = SPACES
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN PRLT-DEDUCT-AMT < 0
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN PRLT-DEDUCT-AMT > 25000.00
                   MOVE 'E591' TO WS-REJECT-CODE
               WHEN PRLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA BAND TIER FOR THE FICA AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-BAND-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLT-FICA-AMT NOT < WS-FICA-BAND-LOW(WS-TX)
                   MOVE WS-FICA-BAND-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-REVIEW-DATE > 0 AND PRLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLT-STATE-TAX-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PRLT-STATE-TAX-AMT - (WS-WORK-AMT *
               2)
           COMPUTE PRLM-GROSS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET YTD FEE ON THE STATE TAX AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3510-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLT-STATE-TAX-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO PRLM-NET-YTD
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO PRLM-NET-YTD
               WHEN OTHER
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLT-STATE-TAX-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-NET-YTD > 0 AND PRLM-NET-YTD < 5.00
               MOVE 5.00 TO PRLM-NET-YTD
           END-IF
           IF PRLM-NET-YTD > 50.00
               MOVE 50.00 TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE NET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLT-NET-AMT NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEDUCT AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-DEDUCT-AMT ROUNDED =
                   PRLT-STATE-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3560-FLAG-NET.
           IF PRLT-NET-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLT-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3570-FLAG-FED-TAX.
           IF PRLM-FED-TAX-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-FED-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-DEDUCT.
           IF PRLM-DEDUCT-AMT < 0.00
               MOVE 0.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-DEDUCT-AMT > 10000.00
               MOVE 10000.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3620-FLAG-STATE-TAX.
           IF PRLM-STATE-TAX-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-STATE-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ST'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E519 NAME REQUIRED
      * E427 DEDUCT AMT NEGATIVE
      * E011 DEDUCT AMT OVER 99999
      * E839 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN PRLT-DEDUCT-AMT < 0
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN PRLT-DEDUCT-AMT > 99999.00
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN PRLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3660-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 100.00
               MOVE 100.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 50000.00
               MOVE 50000.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STATE TAX YTD.
      *----------------------------------------------------------------*
       3680-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-STATE-TAX-YTD ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-STATE-TAX-YTD
           END-IF
           ADD PRLM-STATE-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD PRLM-FED-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E994 CUST ID REQUIRED
      * E356 GROSS YTD NEGATIVE
      * E944 GROSS YTD OVER 10000
      * E075 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CUST-ID = SPACES
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-YTD < 0
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-YTD > 10000.00
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN PRLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PRLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE STATE TAX AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3740-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLT-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-STOP-FLAG = 'Y'
                   COMPUTE PRLT-DEDUCT-AMT ROUNDED = PRLM-STATE-TAX-AMT
                       * PRLM-RATE / 200
               WHEN PRLM-PAY-FREQ-CD = 'JT'
                   MOVE 20.00 TO PRLT-DEDUCT-AMT
               WHEN PRLM-PAY-FREQ-CD = 'XM'
                   MOVE 3.00 TO PRLT-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLT-DEDUCT-AMT ROUNDED = PRLM-STATE-TAX-AMT
                       * PRLM-RATE / 100
           END-EVALUATE
           IF PRLT-DEDUCT-AMT > 0 AND PRLT-DEDUCT-AMT < 10.00
               MOVE 10.00 TO PRLT-DEDUCT-AMT
           END-IF
           IF PRLT-DEDUCT-AMT > 35.00
               MOVE 35.00 TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX AMT BY TAX STATE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-TAX-STATE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-TAX-STATE-CD
                   ADD PRLM-STATE-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-TAX-STATE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-STATE-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3770-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GROSS YTD.
      *----------------------------------------------------------------*
       3780-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-YTD ROUNDED =
                   PRLM-STATE-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-GROSS-YTD
           END-IF
           ADD PRLM-GROSS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN EXPIRY DATE IS PAST
      * PL -> VK WHEN EXPIRY DATE IS PAST
      * VK -> WB WHEN EXPIRY DATE IS PAST
      * WB -> XL WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE STATE TAX YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3810-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PRLT-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLT-FICA-AMT ROUNDED = PRLM-STATE-TAX-YTD *
                       PRLM-MARGIN / 200
               WHEN PRLM-REGION-CD = 'DP'
                   MOVE 15.00 TO PRLT-FICA-AMT
               WHEN PRLM-REGION-CD = 'TU'
                   MOVE 20.00 TO PRLT-FICA-AMT
               WHEN OTHER
                   COMPUTE PRLT-FICA-AMT ROUNDED = PRLM-STATE-TAX-YTD *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLT-FICA-AMT > 0 AND PRLT-FICA-AMT < 10.00
               MOVE 10.00 TO PRLT-FICA-AMT
           END-IF
           IF PRLT-FICA-AMT > 250.00
               MOVE 250.00 TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E453 REF NO REQUIRED
      * E675 NET AMT NEGATIVE
      * E626 NET AMT OVER 50000
      * E176 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 50000.00
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN PRLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E513 MEMO REQUIRED
      * E264 WORK AMT NEGATIVE
      * E601 WORK AMT OVER 50000
      * E965 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN PRLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX AMT BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-PAY-FREQ-CD
                   ADD PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN NOTICE DATE IS PAST
      * LZU -> TUF WHEN NOTICE DATE IS PAST
      * TUF -> WKE WHEN NOTICE DATE IS PAST
      * WKE -> CAZ WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE NET YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3880-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PRLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   COMPUTE PRLM-NET-AMT ROUNDED = PRLM-NET-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-SOURCE-CD = 'NY'
                   MOVE 10.00 TO PRLM-NET-AMT
               WHEN PRLM-SOURCE-CD = 'PL'
                   MOVE 10.00 TO PRLM-NET-AMT
               WHEN OTHER
                   COMPUTE PRLM-NET-AMT ROUNDED = PRLM-NET-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-NET-AMT > 0 AND PRLM-NET-AMT < 2.00
               MOVE 2.00 TO PRLM-NET-AMT
           END-IF
           IF PRLM-NET-AMT > 25.00
               MOVE 25.00 TO PRLM-NET-AMT
           END-IF
           ADD PRLM-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3900-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE FICA YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-FICA-YTD NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E112 CONTACT REQUIRED
      * E887 STATE TAX YTD NEGATIVE
      * E510 STATE TAX YTD OVER 5000
      * E068 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CONTACT = SPACES
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD < 0
                   MOVE 'E887' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD > 5000.00
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GROSS YTD.
      *----------------------------------------------------------------*
       3930-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-YTD ROUNDED =
                   PRLM-NET-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-GROSS-YTD
           END-IF
           ADD PRLM-GROSS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3940-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE STATE TAX YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3950-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLM-STATE-TAX-YTD
                       * PRLM-RATE / 200
               WHEN PRLM-SOURCE-CD = 'NY'
                   MOVE 20.00 TO PRLM-DEDUCT-AMT
               WHEN PRLM-SOURCE-CD = 'PL'
                   MOVE 15.00 TO PRLM-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLM-STATE-TAX-YTD
                       * PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-DEDUCT-AMT > 0 AND PRLM-DEDUCT-AMT < 1.00
               MOVE 1.00 TO PRLM-DEDUCT-AMT
           END-IF
           IF PRLM-DEDUCT-AMT > 35.00
               MOVE 35.00 TO PRLM-DEDUCT-AMT
           END-IF
           ADD PRLM-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E545 CONTACT REQUIRED
      * E759 STATE TAX YTD NEGATIVE
      * E771 STATE TAX YTD OVER 25000
      * E793 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CONTACT = SPACES
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD < 0
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD > 25000.00
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN PRLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3970-FLAG-STATE-TAX.
           IF PRLT-STATE-TAX-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLT-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLM-STATE-TAX-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-STATE-TAX-YTD - (WS-WORK-AMT *
               3)
           COMPUTE PRLT-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 5
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLT-DEDUCT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLT-DEDUCT-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-STATE-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN NOTICE DATE IS PAST
      * PL -> VK WHEN NOTICE DATE IS PAST
      * VK -> WB WHEN NOTICE DATE IS PAST
      * WB -> XL WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLM-STATE-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-STATE-TAX-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLM-STATE-TAX-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-STATE-TAX-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-BAND-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-STATE-TA-BAND-LOW(WS-TX)
                   MOVE WS-STATE-TA-BAND-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEDUCT AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-DEDUCT-AMT ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-DEDUCT-AMT
           END-IF
           ADD PRLM-DEDUCT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4090-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FED TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       4100-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-AMT ROUNDED =
                   PRLM-FED-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-NET-AMT
           END-IF
           ADD PRLM-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE GROSS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEDUCT YTD.
      *----------------------------------------------------------------*
       4110-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-DEDUCT-YTD ROUNDED =
                   PRLM-GROSS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * GROSS YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4120-FLAG-GROSS.
           IF PRLM-GROSS-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE NET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-NET-YTD NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-PYFEE.
           INITIALIZE PYFEE-PARMS
           MOVE PRLM-GROSS-YTD
             TO PYFEE-GROSS-YTD
           MOVE PRLM-DEDUCT-AMT
             TO PYFEE-DEDUCT-AMT
           MOVE PRLM-STATE-TAX-AMT
             TO PYFEE-STATE-TAX-AMT
           MOVE PRLM-NET-AMT
             TO PYFEE-NET-AMT
           MOVE PRLM-STATE-TAX-YTD
             TO PYFEE-STATE-TAX-YTD
           MOVE PRLM-DEDUCT-YTD
             TO PYFEE-DEDUCT-YTD
           MOVE PRLM-NOTICE-DATE
             TO PYFEE-NOTICE-DATE
           MOVE PRLM-LAST-ACT-DATE
             TO PYFEE-LAST-ACT-DATE
           MOVE PRLM-NEXT-DUE-DATE
             TO PYFEE-NEXT-DUE-DATE
           MOVE PRLM-CLOSE-DATE
             TO PYFEE-CLOSE-DATE
           MOVE PRLM-REGION-CD
             TO PYFEE-REGION-CD
           MOVE PRLM-PAY-FREQ-CD
             TO PYFEE-PAY-FREQ-CD
           MOVE PRLM-SOURCE-CD
             TO PYFEE-SOURCE-CD
           MOVE PRLM-TAX-STATE-CD
             TO PYFEE-TAX-STATE-CD
           MOVE PRLM-STOP-FLAG
             TO PYFEE-STOP-FLAG
           MOVE PRLM-AUTO-RENEW-FLAG
             TO PYFEE-AUTO-RENEW-FLAG
           MOVE PRLM-HOLD-FLAG
             TO PYFEE-HOLD-FLAG
           MOVE PRLM-CYCLE-CNT
             TO PYFEE-CYCLE-CNT
           MOVE PRLM-NOTICE-CNT
             TO PYFEE-NOTICE-CNT
           MOVE PRLM-MARGIN
             TO PYFEE-MARGIN
           MOVE PRLM-RATE
             TO PYFEE-RATE
           MOVE PRLM-CONTACT
             TO PYFEE-CONTACT
           MOVE PRLM-NAME
             TO PYFEE-NAME
           CALL 'PYFEE' USING PYFEE-PARMS
           EVALUATE PYFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYFEE-GROSS-YTD
                     TO PRLM-GROSS-YTD
                   MOVE PYFEE-STATE-TAX-AMT
                     TO PRLM-STATE-TAX-AMT
                   MOVE PYFEE-NET-AMT
                     TO PRLM-NET-AMT
                   MOVE PYFEE-STOP-FLAG
                     TO PRLM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-PYFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-PYTIER.
           INITIALIZE PYTIER-PARMS
           MOVE PRLM-DEDUCT-AMT
             TO PYTIER-DEDUCT-AMT
           MOVE PRLM-NET-YTD
             TO PYTIER-NET-YTD
           MOVE PRLM-STATE-TAX-AMT
             TO PYTIER-STATE-TAX-AMT
           MOVE PRLM-NET-AMT
             TO PYTIER-NET-AMT
           MOVE PRLM-GROSS-AMT
             TO PYTIER-GROSS-AMT
           MOVE PRLM-FED-TAX-AMT
             TO PYTIER-FED-TAX-AMT
           MOVE PRLM-NEXT-DUE-DATE
             TO PYTIER-NEXT-DUE-DATE
           MOVE PRLM-EXPIRY-DATE
             TO PYTIER-EXPIRY-DATE
           MOVE PRLM-CLOSE-DATE
             TO PYTIER-CLOSE-DATE
           MOVE PRLM-OPEN-DATE
             TO PYTIER-OPEN-DATE
           MOVE PRLM-PAY-FREQ-CD
             TO PYTIER-PAY-FREQ-CD
           MOVE PRLM-TAX-STATE-CD
             TO PYTIER-TAX-STATE-CD
           MOVE PRLM-STATUS-CD
             TO PYTIER-STATUS-CD
           MOVE PRLM-REASON-CD
             TO PYTIER-REASON-CD
           MOVE PRLM-SUSPEND-FLAG
             TO PYTIER-SUSPEND-FLAG
           MOVE PRLM-HOLD-FLAG
             TO PYTIER-HOLD-FLAG
           MOVE PRLM-PREFERRED-FLAG
             TO PYTIER-PREFERRED-FLAG
           MOVE PRLM-ITEM-CNT
             TO PYTIER-ITEM-CNT
           MOVE PRLM-PAYMENT-CNT
             TO PYTIER-PAYMENT-CNT
           MOVE PRLM-RATE
             TO PYTIER-RATE
           MOVE PRLM-FEE-PCT
             TO PYTIER-FEE-PCT
           MOVE PRLM-DESC
             TO PYTIER-DESC
           MOVE PRLM-CONTACT
             TO PYTIER-CONTACT
           CALL 'PYTIER' USING PYTIER-PARMS
           EVALUATE PYTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYTIER-DEDUCT-AMT
                     TO PRLM-DEDUCT-AMT
                   MOVE PYTIER-STATE-TAX-AMT
                     TO PRLM-STATE-TAX-AMT
                   MOVE PYTIER-FED-TAX-AMT
                     TO PRLM-FED-TAX-AMT
                   MOVE PYTIER-HOLD-FLAG
                     TO PRLM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PYTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PRLT-KEY TO RD-KEY
           MOVE PRLT-TRAN-CD TO RD-CODE
           MOVE PRLT-DEDUCT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PRLT-KEY TO RD-KEY
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
           DISPLAY 'PYLCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PYLCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
