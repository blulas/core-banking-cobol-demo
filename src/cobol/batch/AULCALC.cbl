      *================================================================*
      * PROGRAM  : AULCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DEALER AUTO FINANCE - PERIODIC CALCULATION. READS   *
      *            THE ADL TRANSACTIONS, UPDATES THE ADL MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. INDIRECT AUTO DEALER   *
      *            CONTRACTS, RESERVE AND CHARGEBACK                   *
      * INPUT    : ADLMST (KSDS), ADLTRN, BANKCTL                      *
      * OUTPUT   : ADLOUT, AULCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, AULIM, AUDTE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-05-22 BHS  ORIGINAL                                       *
      * 1996-05-22 JLM  REBATE FEE SCHEDULE UPDATE CHG1496             *
      * 1996-11-21 BHS  AMOUNT-FIN LIMIT REVISED PRB1384               *
      * 1996-10-10 PQR  GAP STATUS CODES ADDED REQ6908                 *
      * 1997-05-01 JLM  PAYMENT CALCULATION CORRECTED REQ2335          *
      * 1999-10-15 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-18 SKN  AUDIT FINDING - AMOUNT-FIN CONTROL TOTALS CHG63*
      * 2001-04-18 SKN  NEW RESERVE REPORT COLUMNS REQ0176             *
      * 2003-09-04 TWH  GAP RERUN PROTECTION PRB0249                   *
      * 2004-04-20 GAF  AMOUNT-FIN RERUN PROTECTION INC5159            *
      * 2005-10-10 GAF  PAYMENT ROUNDING - HALF UP CHG1685             *
      * 2006-04-22 EWS  PERFORMANCE - RESERVE READ AHEAD PRB8315       *
      * 2010-01-03 GAF  CHARGEBK STATUS CODES ADDED REQ6824            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULCALC.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ADLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ADLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ADLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ADLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "AULCARPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ADLMST.
       FD  TRAN-FILE.
       COPY ADLTRN.
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
                                           VALUE 15.000000.
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
      * WS-GAP-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GAP-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000425500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000895000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000544000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000838500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000892000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022600000000546000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023600000000590000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024600000000016000'.
       01  WS-GAP-GRID-TBL REDEFINES WS-GAP-GRID-VALUES.
           05  WS-GAP-GRID-ROW OCCURS 8 TIMES.
               10  WS-GAP-GRID-LOW         PIC 9(09)V99.
               10  WS-GAP-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000804000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000224000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000859000'.
       01  WS-PAYMENT-SCHED-TBL REDEFINES WS-PAYMENT-SCHED-VALUES.
           05  WS-PAYMENT-SCHED-ROW OCCURS 4 TIMES.
               10  WS-PAYMENT-SCHED-LOW    PIC 9(09)V99.
               10  WS-PAYMENT-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000005000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000506500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000439500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000418500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000881500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000383000'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 6 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  AULCALC  DEALER AUTO FINANCE'.
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
       COPY AULIMP.
       COPY AUDTEP.

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
               MOVE 'ADLMST' TO ABND-FILE-NAME
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
           MOVE ADLT-KEY TO ADLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-STATUS
               PERFORM 3010-EDIT-MEMO
               PERFORM 3020-SPLIT-GAP
               PERFORM 3030-ACCRUE-PAYMENT
               PERFORM 3040-SELECT-RATE
               PERFORM 3050-COUNT-SEQ
               PERFORM 3060-PRORATE-RESERVE
               PERFORM 3070-FLAG-RESERVE
               PERFORM 3080-DESCRIBE-CHANNEL
               PERFORM 3090-SET-TIER
               PERFORM 3100-CHECK-POST
               PERFORM 3110-SET-REASON
               PERFORM 3120-EDIT-ADDR
               PERFORM 3130-AGE-NOTICE
               PERFORM 3140-DESCRIBE-TYPE
               PERFORM 3150-CHECK-NOTICE
               PERFORM 3160-ACCRUE-REBATE
               PERFORM 3170-SPLIT-GAP
               PERFORM 3180-SELECT-FEE-PCT
               PERFORM 3190-AGE-POST
               PERFORM 3200-SELECT-FEE-PCT
               PERFORM 3210-COUNT-SEQ
               PERFORM 3220-ACCUM-REASON
               PERFORM 3230-EDIT-CUST-ID
               PERFORM 3240-EDIT-ADDR
               PERFORM 3250-ACCUM-REASON
               PERFORM 3260-COUNT-PAYMENT
               PERFORM 3270-LIMIT-AMOUNT-FIN
               PERFORM 3280-AGE-NEXT-DUE
               PERFORM 3290-PRORATE-PAYMENT
               PERFORM 3300-CHECK-EXPIRY
               PERFORM 3310-ASSESS-OVERRIDE
               PERFORM 3320-SELECT-FEE-PCT
               PERFORM 3330-LIMIT-RESERVE
               PERFORM 3340-DESCRIBE-TIER
               PERFORM 3350-SET-DEALER
               PERFORM 3360-DESCRIBE-CHANNEL
               PERFORM 3370-CHECK-NEXT-REVIEW
               PERFORM 3380-PRORATE-REBATE
               PERFORM 3390-DESCRIBE-REASON
               PERFORM 3400-FLAG-AMOUNT-FIN
               PERFORM 3410-PRORATE-GAP
               PERFORM 3420-DESCRIBE-TRAN
               PERFORM 3430-ACCRUE-REBATE
               PERFORM 3440-FLAG-PAYMENT
               PERFORM 3450-CHECK-LAST-MAINT
               PERFORM 3460-SELECT-FEE-PCT
               PERFORM 3470-FLAG-REBATE
               PERFORM 3480-ACCUM-CYCLE
               PERFORM 3490-LIMIT-ITEM
               PERFORM 3500-FLAG-CHARGEBK
               PERFORM 3510-ACCUM-CHANNEL
               PERFORM 3520-DESCRIBE-TRAN
               PERFORM 3530-PRORATE-REBATE
               PERFORM 3540-SPLIT-REBATE
               PERFORM 3550-CHECK-LAST-ACT
               PERFORM 3560-SPLIT-RESERVE
               PERFORM 3570-ACCRUE-RESERVE
               PERFORM 3580-ACCUM-REASON
               PERFORM 3590-SELECT-RATE
               PERFORM 3600-LIMIT-CHARGEBK
               PERFORM 3610-AGE-LAST-ACT
               PERFORM 3620-COUNT-CYCLE
               PERFORM 3630-LIMIT-CHARGEBK
               PERFORM 3640-SPLIT-RESERVE
               PERFORM 3650-SELECT-RATE
               PERFORM 3660-LIMIT-GAP
               PERFORM 3670-LIMIT-RESERVE
               PERFORM 3680-PRORATE-REBATE
               PERFORM 3690-ASSESS-REVERSAL
               PERFORM 3700-SPLIT-GAP
               PERFORM 3710-SPLIT-PAYMENT
               PERFORM 3720-ASSESS-REVERSAL
               PERFORM 3730-FLAG-PAYMENT
               PERFORM 3740-COUNT-PAYMENT
               PERFORM 3750-COUNT-RETRY
               PERFORM 3760-SET-TRAN
               PERFORM 3770-SELECT-MARGIN
               PERFORM 3780-ACCUM-TYPE
               PERFORM 3790-SPLIT-AMOUNT-FIN
               PERFORM 3800-LIMIT-AMOUNT-FIN
               PERFORM 3810-SELECT-MARGIN
               PERFORM 3820-ACCRUE-MARGIN
               PERFORM 3830-SELECT-RATE
               PERFORM 3840-EDIT-CONTACT
               PERFORM 3850-AGE-LAST-STMT
               PERFORM 3860-ACCRUE-RESERVE
               PERFORM 3870-COUNT-RETRY
               PERFORM 3880-SET-TYPE
               PERFORM 3890-SPLIT-REBATE
               PERFORM 3900-SPLIT-REBATE
               PERFORM 3910-ACCUM-CHANNEL
               PERFORM 3920-SELECT-FEE-PCT
               PERFORM 3930-PRORATE-RESERVE
               PERFORM 3940-ACCUM-CYCLE
               PERFORM 3950-ACCUM-REASON
               PERFORM 3960-SELECT-FEE-PCT
               PERFORM 3970-SELECT-MARGIN
               PERFORM 3980-SELECT-FEE-PCT
               PERFORM 3990-SELECT-FEE-PCT
               PERFORM 4000-SELECT-FEE-PCT
               PERFORM 4010-PRORATE-PAYMENT
               PERFORM 4020-ASSESS-REVIEW
               PERFORM 4030-SELECT-MARGIN
               PERFORM 4040-SET-STATUS
               PERFORM 4050-LIMIT-AMOUNT-FIN
               PERFORM 4060-DESCRIBE-REASON
               PERFORM 4070-CHECK-LAST-ACT
               PERFORM 7000-CALL-AULIM
               PERFORM 7020-CALL-AUDTE
               REWRITE ADL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ADLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ADL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN EXPIRY DATE IS PAST
      * HMW -> RCB WHEN EXPIRY DATE IS PAST
      * RCB -> EUA WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HMW' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RCB' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E213 MEMO REQUIRED
      * E940 REBATE AMT NEGATIVE
      * E345 REBATE AMT OVER 99999
      * E712 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3010-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-MEMO = SPACES
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN ADLT-REBATE-AMT < 0
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN ADLT-REBATE-AMT > 99999.00
                   MOVE 'E345' TO WS-REJECT-CODE
               WHEN ADLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GAP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = ADLM-GAP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ADLM-GAP-YTD - (WS-WORK-AMT * 4)
           COMPUTE ADLT-REBATE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLM-GAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-CHARGEBK-YTD NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 10
               MOVE 'Y' TO ADLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESERVE YTD.
      *----------------------------------------------------------------*
       3060-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-RESERVE-YTD ROUNDED =
                   ADLM-GAP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-RESERVE-YTD
           END-IF
           ADD ADLM-RESERVE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESERVE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3070-FLAG-RESERVE.
           IF ADLM-RESERVE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-RESERVE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN NEXT DUE DATE IS PAST
      * JF -> KD WHEN NEXT DUE DATE IS PAST
      * KD -> UG WHEN NEXT DUE DATE IS PAST
      * UG -> WY WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ADLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN LAST PAY DATE IS PAST
      * KPP -> MVN WHEN LAST PAY DATE IS PAST
      * MVN -> UVY WHEN LAST PAY DATE IS PAST
      * UVY -> ZWZ WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E164 ADDR REQUIRED
      * E499 PAYMENT AMT NEGATIVE
      * E450 PAYMENT AMT OVER 50000
      * E173 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-ADDR = SPACES
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-AMT < 0
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-AMT > 50000.00
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN ADLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-REBATE.
           IF ADLM-REBATE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-REBATE-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-RESERVE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE ADLM-GAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ADLM-GAP-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ADLT-RESERVE-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 5
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E737 CUST ID REQUIRED
      * E792 RESERVE AMT NEGATIVE
      * E162 RESERVE AMT OVER 5000
      * E875 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-CUST-ID = SPACES
                   MOVE 'E737' TO WS-REJECT-CODE
               WHEN ADLT-RESERVE-AMT < 0
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN ADLT-RESERVE-AMT > 5000.00
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN ADLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E937 ADDR REQUIRED
      * E413 AMOUNT FIN AMT NEGATIVE
      * E765 AMOUNT FIN AMT OVER 99999
      * E376 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-ADDR = SPACES
                   MOVE 'E937' TO WS-REJECT-CODE
               WHEN ADLM-AMOUNT-FIN-AMT < 0
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN ADLM-AMOUNT-FIN-AMT > 99999.00
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN ADLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GAP AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLM-GAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-GAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-PAYMENT.
           IF ADLM-PAYMENT-CNT < 9999999
               ADD 1 TO ADLM-PAYMENT-CNT
           END-IF
           IF ADLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3270-LIMIT-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT < 0.00
               MOVE 0.00 TO ADLT-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLT-AMOUNT-FIN-AMT > 500.00
               MOVE 500.00 TO ADLT-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3290-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-AMT ROUNDED =
                   ADLM-AMOUNT-FIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3310-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ADLM-CHARGEBK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ADLM-CHARGEBK-AMT ROUNDED = WS-WORK-AMT-2 *
                       ADLM-FEE-PCT / 200
               WHEN ADLT-TRAN-CD = 'BK'
                   MOVE 7.00 TO ADLM-CHARGEBK-AMT
               WHEN ADLT-TRAN-CD = 'PY'
                   MOVE 15.00 TO ADLM-CHARGEBK-AMT
               WHEN OTHER
                   COMPUTE ADLM-CHARGEBK-AMT ROUNDED = WS-WORK-AMT-2 *
                       ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLM-CHARGEBK-AMT > 0 AND ADLM-CHARGEBK-AMT < 1.00
               MOVE 1.00 TO ADLM-CHARGEBK-AMT
           END-IF
           IF ADLM-CHARGEBK-AMT > 250.00
               MOVE 250.00 TO ADLM-CHARGEBK-AMT
           END-IF
           ADD ADLM-CHARGEBK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ADLM-REBATE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3330-LIMIT-RESERVE.
           IF ADLT-RESERVE-AMT < 100.00
               MOVE 100.00 TO ADLT-RESERVE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLT-RESERVE-AMT > 500.00
               MOVE 500.00 TO ADLT-RESERVE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN EFF DATE IS PAST
      * FNF -> GBX WHEN EFF DATE IS PAST
      * GBX -> GTU WHEN EFF DATE IS PAST
      * GTU -> JNE WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNF' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GBX' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GTU' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNE' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-REBATE-AMT ROUNDED =
                   ADLM-RESERVE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KPP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MVN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UVY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZWZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT FIN YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3400-FLAG-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-AMOUNT-FIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GAP AMT.
      *----------------------------------------------------------------*
       3410-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-GAP-AMT ROUNDED =
                   ADLT-RESERVE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-GAP-AMT
           END-IF
           ADD ADLM-GAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-REBATE.
           IF ADLT-REBATE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-REBATE-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-CHARGEBK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3440-FLAG-PAYMENT.
           IF ADLM-PAYMENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ADLT-REBATE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REBATE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3470-FLAG-REBATE.
           IF ADLM-REBATE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-REBATE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD ADLM-CHARGEBK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-CHARGEBK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3490-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3500-FLAG-CHARGEBK.
           IF ADLM-CHARGEBK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-CHARGEBK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-CHANNEL-CD
                   ADD ADLM-CHARGEBK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-CHARGEBK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-REBATE-AMT ROUNDED =
                   ADLM-CHARGEBK-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-AMT - (WS-WORK-AMT * 4)
           COMPUTE ADLT-AMOUNT-FIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESERVE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLM-RESERVE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ADLM-RESERVE-YTD - (WS-WORK-AMT * 2)
           COMPUTE ADLM-GAP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-RESERVE.
           IF ADLT-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-RESERVE-AMT * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3600-LIMIT-CHARGEBK.
           IF ADLM-CHARGEBK-AMT < 25.00
               MOVE 25.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-CHARGEBK-AMT > 5000.00
               MOVE 5000.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3630-LIMIT-CHARGEBK.
           IF ADLM-CHARGEBK-AMT < 10.00
               MOVE 10.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-CHARGEBK-AMT > 2500.00
               MOVE 2500.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESERVE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLM-RESERVE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-RESERVE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ADLM-GAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GAP YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3660-LIMIT-GAP.
           IF ADLM-GAP-YTD < 100.00
               MOVE 100.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-GAP-YTD > 5000.00
               MOVE 5000.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-RESERVE.
           IF ADLM-RESERVE-AMT < 100.00
               MOVE 100.00 TO ADLM-RESERVE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-RESERVE-AMT > 1000.00
               MOVE 1000.00 TO ADLM-RESERVE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-REBATE-AMT ROUNDED =
                   ADLM-RESERVE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK FEE ON THE RESERVE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3690-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ADLM-CHARGEBK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ADLM-CHARGEBK-AMT ROUNDED = ADLT-RESERVE-AMT
                       * ADLM-RATE / 200
               WHEN ADLM-DEALER-CD = 'DRU'
                   MOVE 10.00 TO ADLM-CHARGEBK-AMT
               WHEN ADLM-DEALER-CD = 'FNF'
                   MOVE 3.00 TO ADLM-CHARGEBK-AMT
               WHEN OTHER
                   COMPUTE ADLM-CHARGEBK-AMT ROUNDED = ADLT-RESERVE-AMT
                       * ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-CHARGEBK-AMT > 0 AND ADLM-CHARGEBK-AMT < 10.00
               MOVE 10.00 TO ADLM-CHARGEBK-AMT
           END-IF
           IF ADLM-CHARGEBK-AMT > 100.00
               MOVE 100.00 TO ADLM-CHARGEBK-AMT
           END-IF
           ADD ADLM-CHARGEBK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT GAP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = ADLM-GAP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-GAP-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ADLM-PAYMENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-PAYMENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE ADLM-AMOUNT-FIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE YTD FEE ON THE RESERVE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3720-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ADLM-RESERVE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLM-RESERVE-YTD ROUNDED = ADLM-RESERVE-AMT *
                       ADLM-MARGIN / 200
               WHEN ADLM-STATUS-CD = 'EUA'
                   MOVE 7.00 TO ADLM-RESERVE-YTD
               WHEN ADLM-STATUS-CD = 'HMW'
                   MOVE 3.00 TO ADLM-RESERVE-YTD
               WHEN OTHER
                   COMPUTE ADLM-RESERVE-YTD ROUNDED = ADLM-RESERVE-AMT *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-RESERVE-YTD > 0 AND ADLM-RESERVE-YTD < 2.00
               MOVE 2.00 TO ADLM-RESERVE-YTD
           END-IF
           IF ADLM-RESERVE-YTD > 250.00
               MOVE 250.00 TO ADLM-RESERVE-YTD
           END-IF
           ADD ADLM-RESERVE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-PAYMENT.
           IF ADLT-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-PAYMENT.
           IF ADLM-PAYMENT-CNT < 9999999
               ADD 1 TO ADLM-PAYMENT-CNT
           END-IF
           IF ADLM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO ADLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BK -> PY WHEN LAST STMT DATE IS PAST
      * PY -> CB WHEN LAST STMT DATE IS PAST
      * CB -> RB WHEN LAST STMT DATE IS PAST
      * RB -> GP WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CB'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE AMOUNT FIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ADLM-AMOUNT-FIN-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TYPE-CD
                   ADD ADLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = ADLM-AMOUNT-FIN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-AMOUNT-FIN-YTD - (WS-WORK-AMT *
               3)
           COMPUTE ADLM-REBATE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT < 10.00
               MOVE 10.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-AMT > 50000.00
               MOVE 50000.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-GAP-AMT NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLM-GAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-AMOUNT-FIN-YTD NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E999 CONTACT REQUIRED
      * E003 REBATE AMT NEGATIVE
      * E477 REBATE AMT OVER 10000
      * E173 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-CONTACT = SPACES
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN ADLT-REBATE-AMT < 0
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN ADLT-REBATE-AMT > 10000.00
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN ADLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-RESERVE.
           IF ADLM-RESERVE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-YTD * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO ADLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN NOTICE DATE IS PAST
      * H -> S WHEN NOTICE DATE IS PAST
      * S -> V WHEN NOTICE DATE IS PAST
      * V -> W WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE ADLT-REBATE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLT-REBATE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLT-REBATE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ADLM-CHARGEBK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN YTD BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-CHANNEL-CD
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLT-PAYMENT-AMT NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3930-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ADLM-RESERVE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE GAP AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD ADLM-GAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-GAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REBATE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLM-REBATE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-REBATE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ADLM-RESERVE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ADLT-PAYMENT-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ADLM-REBATE-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP GRID TIER FOR THE AMOUNT FIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ADLM-AMOUNT-FIN-AMT NOT < WS-GAP-GRID-LOW(WS-TX)
                   MOVE WS-GAP-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE CHARGEBK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ADLM-CHARGEBK-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-AMT ROUNDED =
                   ADLM-AMOUNT-FIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE YTD FEE ON THE AMOUNT FIN YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4020-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLM-RESERVE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLM-RESERVE-YTD ROUNDED =
                       ADLM-AMOUNT-FIN-YTD * ADLM-MARGIN / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 3.00 TO ADLM-RESERVE-YTD
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 3.00 TO ADLM-RESERVE-YTD
               WHEN OTHER
                   COMPUTE ADLM-RESERVE-YTD ROUNDED =
                       ADLM-AMOUNT-FIN-YTD * ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-RESERVE-YTD > 0 AND ADLM-RESERVE-YTD < 2.00
               MOVE 2.00 TO ADLM-RESERVE-YTD
           END-IF
           IF ADLM-RESERVE-YTD > 35.00
               MOVE 35.00 TO ADLM-RESERVE-YTD
           END-IF
           ADD ADLM-RESERVE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-RESERVE-AMT NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN LAST PAY DATE IS PAST
      * HMW -> RCB WHEN LAST PAY DATE IS PAST
      * RCB -> EUA WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HMW' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RCB' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4050-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT < 100.00
               MOVE 100.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-AMT > 2500.00
               MOVE 2500.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KPP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MVN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UVY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZWZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ADL FIELDS TO AULIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-AULIM.
           INITIALIZE AULIM-PARMS
           MOVE ADLM-REBATE-YTD
             TO AULIM-REBATE-YTD
           MOVE ADLM-CHARGEBK-YTD
             TO AULIM-CHARGEBK-YTD
           MOVE ADLM-REBATE-AMT
             TO AULIM-REBATE-AMT
           MOVE ADLM-PAYMENT-AMT
             TO AULIM-PAYMENT-AMT
           MOVE ADLM-CHARGEBK-AMT
             TO AULIM-CHARGEBK-AMT
           MOVE ADLM-AMOUNT-FIN-YTD
             TO AULIM-AMOUNT-FIN-YTD
           MOVE ADLM-LAST-MAINT-DATE
             TO AULIM-LAST-MAINT-DATE
           MOVE ADLM-LAST-STMT-DATE
             TO AULIM-LAST-STMT-DATE
           MOVE ADLM-NEXT-DUE-DATE
             TO AULIM-NEXT-DUE-DATE
           MOVE ADLM-LAST-PAY-DATE
             TO AULIM-LAST-PAY-DATE
           MOVE ADLM-TIER-CD
             TO AULIM-TIER-CD
           MOVE ADLM-TYPE-CD
             TO AULIM-TYPE-CD
           MOVE ADLM-STATUS-CD
             TO AULIM-STATUS-CD
           MOVE ADLM-DEALER-CD
             TO AULIM-DEALER-CD
           MOVE ADLM-EXEMPT-FLAG
             TO AULIM-EXEMPT-FLAG
           MOVE ADLM-STOP-FLAG
             TO AULIM-STOP-FLAG
           MOVE ADLM-OVERRIDE-FLAG
             TO AULIM-OVERRIDE-FLAG
           MOVE ADLM-CYCLE-CNT
             TO AULIM-CYCLE-CNT
           MOVE ADLM-PAYMENT-CNT
             TO AULIM-PAYMENT-CNT
           MOVE ADLM-MARGIN
             TO AULIM-MARGIN
           MOVE ADLM-FEE-PCT
             TO AULIM-FEE-PCT
           MOVE ADLM-ADDR
             TO AULIM-ADDR
           MOVE ADLM-NAME
             TO AULIM-NAME
           CALL 'AULIM' USING AULIM-PARMS
           EVALUATE AULIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE AULIM-REBATE-AMT
                     TO ADLM-REBATE-AMT
                   MOVE AULIM-PAYMENT-AMT
                     TO ADLM-PAYMENT-AMT
                   MOVE AULIM-STOP-FLAG
                     TO ADLM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-AULIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ADL FIELDS TO AUDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-AUDTE.
           INITIALIZE AUDTE-PARMS
           MOVE ADLM-PAYMENT-YTD
             TO AUDTE-PAYMENT-YTD
           MOVE ADLM-REBATE-YTD
             TO AUDTE-REBATE-YTD
           MOVE ADLM-REBATE-AMT
             TO AUDTE-REBATE-AMT
           MOVE ADLM-AMOUNT-FIN-AMT
             TO AUDTE-AMOUNT-FIN-AMT
           MOVE ADLM-PAYMENT-AMT
             TO AUDTE-PAYMENT-AMT
           MOVE ADLM-GAP-AMT
             TO AUDTE-GAP-AMT
           MOVE ADLM-NOTICE-DATE
             TO AUDTE-NOTICE-DATE
           MOVE ADLM-LAST-MAINT-DATE
             TO AUDTE-LAST-MAINT-DATE
           MOVE ADLM-EFFECTIVE-DATE
             TO AUDTE-EFFECTIVE-DATE
           MOVE ADLM-EXPIRY-DATE
             TO AUDTE-EXPIRY-DATE
           MOVE ADLM-TYPE-CD
             TO AUDTE-TYPE-CD
           MOVE ADLM-TIER-CD
             TO AUDTE-TIER-CD
           MOVE ADLM-DEALER-CD
             TO AUDTE-DEALER-CD
           MOVE ADLM-CYCLE-CD
             TO AUDTE-CYCLE-CD
           MOVE ADLM-OVERRIDE-FLAG
             TO AUDTE-OVERRIDE-FLAG
           MOVE ADLM-NOTICE-SENT-FLAG
             TO AUDTE-NOTICE-SENT-FLAG
           MOVE ADLM-REVIEW-FLAG
             TO AUDTE-REVIEW-FLAG
           MOVE ADLM-RETRY-CNT
             TO AUDTE-RETRY-CNT
           MOVE ADLM-EXCEPT-CNT
             TO AUDTE-EXCEPT-CNT
           MOVE ADLM-FEE-PCT
             TO AUDTE-FEE-PCT
           MOVE ADLM-MARGIN
             TO AUDTE-MARGIN
           MOVE ADLM-DESC
             TO AUDTE-DESC
           MOVE ADLM-NAME
             TO AUDTE-NAME
           CALL 'AUDTE' USING AUDTE-PARMS
           EVALUATE AUDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE AUDTE-PAYMENT-YTD
                     TO ADLM-PAYMENT-YTD
                   MOVE AUDTE-REBATE-YTD
                     TO ADLM-REBATE-YTD
                   MOVE AUDTE-REBATE-AMT
                     TO ADLM-REBATE-AMT
                   MOVE AUDTE-GAP-AMT
                     TO ADLM-GAP-AMT
                   MOVE AUDTE-REVIEW-FLAG
                     TO ADLM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-AUDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ADLT-KEY TO RD-KEY
           MOVE ADLT-TRAN-CD TO RD-CODE
           MOVE ADLT-PAYMENT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ADLT-KEY TO RD-KEY
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
           DISPLAY 'AULCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'AULCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
