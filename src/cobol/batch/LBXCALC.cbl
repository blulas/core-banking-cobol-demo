      *================================================================*
      * PROGRAM  : LBXCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : LOCKBOX REMITTANCE - PERIODIC CALCULATION. READS THE*
      *            LBX TRANSACTIONS, UPDATES THE LBX MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. WHOLESALE LOCKBOX      *
      *            REMITTANCE CAPTURE AND CREDIT TO CLIENTS            *
      * INPUT    : LBXMST (KSDS), LBXTRN, BANKCTL                      *
      * OUTPUT   : LBXOUT, LBXCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, LBFEE, LBWAIV, LBCAL    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-08 DPS  ORIGINAL                                       *
      * 1997-06-22 SKN  CASH-ON-HAND FEE SCHEDULE UPDATE PRB7893       *
      * 2004-01-28 GAF  REMIT ROUNDING - HALF UP PRB8162               *
      * 2007-10-07 PQR  FLOAT STATUS CODES ADDED PRB2776               *
      * 2009-03-08 MKL  CHECK LIMIT REVISED CHG0311                    *
      * 2009-11-14 GAF  PERFORMANCE - EXCEPT READ AHEAD REQ6532        *
      * 2012-03-06 DPS  FLOAT RERUN PROTECTION PRB2787                 *
      * 2014-01-19 EWS  FLOAT LIMIT REVISED INC9106                    *
      * 2014-10-26 SKN  EXCEPT LIMIT REVISED CHG9952                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBXCALC.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "LBXMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LBXM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "LBXTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "LBXOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "LBXCARPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY LBXMST.
       FD  TRAN-FILE.
       COPY LBXTRN.
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
      * WS-EXCEPT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCEPT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000473500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000054000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000072500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000199500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000567000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018000000000874500'.
       01  WS-EXCEPT-SCHED-TBL REDEFINES WS-EXCEPT-SCHED-VALUES.
           05  WS-EXCEPT-SCHED-ROW OCCURS 6 TIMES.
               10  WS-EXCEPT-SCHED-LOW     PIC 9(09)V99.
               10  WS-EXCEPT-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEPOSIT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000368500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000680500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000504000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000553000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004250000000373500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014250000000401000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014750000000608500'.
       01  WS-DEPOSIT-GRID-TBL REDEFINES WS-DEPOSIT-GRID-VALUES.
           05  WS-DEPOSIT-GRID-ROW OCCURS 7 TIMES.
               10  WS-DEPOSIT-GRID-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FLOAT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FLOAT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000296500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000588000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000177500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000533500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000091500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022800000000059000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032800000000802000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033300000000338500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035800000000487500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038300000000300500'.
       01  WS-FLOAT-GRID-TBL REDEFINES WS-FLOAT-GRID-VALUES.
           05  WS-FLOAT-GRID-ROW OCCURS 10 TIMES.
               10  WS-FLOAT-GRID-LOW       PIC 9(09)V99.
               10  WS-FLOAT-GRID-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  LBXCALC  LOCKBOX REMITTANCE'.
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
       COPY LBFEEP.
       COPY LBWAIVP.
       COPY LBCALP.

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
               MOVE 'LBXMST' TO ABND-FILE-NAME
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
           MOVE LBXT-KEY TO LBXM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-REGION
               PERFORM 3010-ACCUM-LOCKBOX
               PERFORM 3020-SET-CHANNEL
               PERFORM 3030-AGE-NEXT-DUE
               PERFORM 3040-SPLIT-CHECK
               PERFORM 3050-SET-CHANNEL
               PERFORM 3060-CHECK-MATURITY
               PERFORM 3070-COUNT-SEQ
               PERFORM 3080-EDIT-REJECT
               PERFORM 3090-SET-LOCKBOX
               PERFORM 3100-PRORATE-CASH-ON-HAND
               PERFORM 3110-LIMIT-FEE
               PERFORM 3120-CHECK-NEXT-DUE
               PERFORM 3130-ACCUM-CYCLE
               PERFORM 3140-SELECT-PENALTY-PCT
               PERFORM 3150-SPLIT-FEE
               PERFORM 3160-LIMIT-CASH-ON-HAND
               PERFORM 3170-SPLIT-REMIT
               PERFORM 3180-COUNT-ITEM
               PERFORM 3190-EDIT-NAME
               PERFORM 3200-CHECK-CLOSE
               PERFORM 3210-CHECK-LAST-STMT
               PERFORM 3220-CHECK-MATURITY
               PERFORM 3230-SET-REGION
               PERFORM 3240-ACCUM-REGION
               PERFORM 3250-DESCRIBE-BATCH-TYPE
               PERFORM 3260-FLAG-FLOAT
               PERFORM 3270-SPLIT-EXCEPT
               PERFORM 3280-LIMIT-REMIT
               PERFORM 3290-PRORATE-FLOAT
               PERFORM 3300-SPLIT-CHECK
               PERFORM 3310-ASSESS-REVERSAL
               PERFORM 3320-SPLIT-CASH-ON-HAND
               PERFORM 3330-PRORATE-REMIT
               PERFORM 3340-FLAG-EXCEPT
               PERFORM 3350-COUNT-RETRY
               PERFORM 3360-EDIT-MEMO
               PERFORM 3370-SPLIT-EXCEPT
               PERFORM 3380-ACCRUE-CASH-ON-HAND
               PERFORM 3390-LIMIT-FEE
               PERFORM 3400-SELECT-PENALTY-PCT
               PERFORM 3410-SELECT-FEE-PCT
               PERFORM 3420-SPLIT-DEPOSIT
               PERFORM 3430-SPLIT-FEE
               PERFORM 3440-ACCUM-CYCLE
               PERFORM 3450-ACCUM-CHANNEL
               PERFORM 3460-EDIT-ADDR
               PERFORM 3470-AGE-EXPIRY
               PERFORM 3480-ACCUM-REGION
               PERFORM 3490-PRORATE-CHECK
               PERFORM 3500-DESCRIBE-CYCLE
               PERFORM 3510-DESCRIBE-CHANNEL
               PERFORM 3520-ASSESS-EXEMPT
               PERFORM 3530-SELECT-FEE-PCT
               PERFORM 3540-AGE-NOTICE
               PERFORM 3550-EDIT-CONTACT
               PERFORM 3560-SPLIT-CHECK
               PERFORM 3570-EDIT-CONTACT
               PERFORM 3580-ACCRUE-PENALTY-PCT
               PERFORM 3590-CHECK-OPEN
               PERFORM 3600-ACCUM-CHANNEL
               PERFORM 3610-ACCUM-CHANNEL
               PERFORM 3620-LIMIT-DEPOSIT
               PERFORM 3630-COUNT-SEQ
               PERFORM 3640-SPLIT-FLOAT
               PERFORM 3650-COUNT-SEQ
               PERFORM 3660-ACCUM-CYCLE
               PERFORM 3670-SELECT-DISCOUNT
               PERFORM 3680-AGE-EFF
               PERFORM 3690-SET-REASON
               PERFORM 3700-SET-TRAN
               PERFORM 3710-LIMIT-FEE
               PERFORM 3720-EDIT-DESC
               PERFORM 3730-DESCRIBE-REASON
               PERFORM 3740-SET-CHANNEL
               PERFORM 3750-ASSESS-HOLD
               PERFORM 3760-ACCRUE-DEPOSIT
               PERFORM 3770-EDIT-CUST-ID
               PERFORM 3780-SPLIT-FLOAT
               PERFORM 3790-COUNT-SEQ
               PERFORM 3800-LIMIT-FLOAT
               PERFORM 3810-ACCUM-CYCLE
               PERFORM 3820-FLAG-CASH-ON-HAND
               PERFORM 3830-SET-REASON
               PERFORM 3840-COUNT-ITEM
               PERFORM 3850-DESCRIBE-CHANNEL
               PERFORM 3860-ACCUM-TRAN
               PERFORM 3870-LIMIT-CASH-ON-HAND
               PERFORM 3880-ASSESS-EXEMPT
               PERFORM 3890-CHECK-LAST-STMT
               PERFORM 3900-DESCRIBE-CHANNEL
               PERFORM 3910-FLAG-FEE
               PERFORM 3920-PRORATE-DEPOSIT
               PERFORM 3930-DESCRIBE-REGION
               PERFORM 3940-COUNT-PAYMENT
               PERFORM 3950-SELECT-PENALTY-PCT
               PERFORM 3960-SELECT-DISCOUNT
               PERFORM 3970-DESCRIBE-REGION
               PERFORM 3980-ACCRUE-EXCEPT
               PERFORM 3990-CHECK-OPEN
               PERFORM 4000-PRORATE-FLOAT
               PERFORM 4010-LIMIT-FLOAT
               PERFORM 4020-CHECK-NEXT-DUE
               PERFORM 4030-PRORATE-DEPOSIT
               PERFORM 4040-DESCRIBE-CHANNEL
               PERFORM 4050-FLAG-FLOAT
               PERFORM 4060-AGE-EFFECTIVE
               PERFORM 4070-CHECK-NOTICE
               PERFORM 4080-LIMIT-FLOAT
               PERFORM 4090-LIMIT-CHECK
               PERFORM 4100-COUNT-SEQ
               PERFORM 4110-SELECT-FEE-PCT
               PERFORM 4120-ASSESS-HOLD
               PERFORM 4130-LIMIT-DEPOSIT
               PERFORM 4140-AGE-EFF
               PERFORM 4150-ASSESS-STOP
               PERFORM 4160-ACCRUE-PENALTY-PCT
               PERFORM 4170-LIMIT-CHECK
               PERFORM 4180-LIMIT-CASH-ON-HAND
               PERFORM 4190-ASSESS-HOLD
               PERFORM 4200-CHECK-CLOSE
               PERFORM 4210-DESCRIBE-REASON
               PERFORM 4220-FLAG-REMIT
               PERFORM 7020-CALL-LBFEE
               PERFORM 7000-CALL-LBWAIV
               PERFORM 7010-CALL-LBCAL
               REWRITE LBX-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'LBXMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM LBX-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-LOCKBOX-CD
                   ADD LBXM-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHECK YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXM-CHECK-YTD / 4
           COMPUTE WS-WORK-AMT-2 = LBXM-CHECK-YTD - (WS-WORK-AMT * 4)
           COMPUTE LBXM-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 12
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E201 REJECT CD REQUIRED
      * E156 FEE YTD NEGATIVE
      * E731 FEE YTD OVER 250000
      * E026 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-REJECT-CD = SPACES
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD < 0
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD > 250000.00
                   MOVE 'E731' TO WS-REJECT-CODE
               WHEN LBXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN LAST STMT DATE IS PAST
      * JM -> WW WHEN LAST STMT DATE IS PAST
      * WW -> XE WHEN LAST STMT DATE IS PAST
      * XE -> DX WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ON HAND AMT.
      *----------------------------------------------------------------*
       3100-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                   LBXT-CHECK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3110-LIMIT-FEE.
           IF LBXT-FEE-AMT < 50.00
               MOVE 50.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FEE-AMT > 10000.00
               MOVE 10000.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT SCHED TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-SCHED-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-DEPOSIT-YTD NOT < WS-EXCEPT-SCHED-LOW(WS-TX)
                   MOVE WS-EXCEPT-SCHED-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXT-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXT-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE LBXM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT < 100.00
               MOVE 100.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 5000.00
               MOVE 5000.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 3
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E494 NAME REQUIRED
      * E074 EXCEPT YTD NEGATIVE
      * E125 EXCEPT YTD OVER 99999
      * E018 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-NAME = SPACES
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-YTD < 0
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN LBXM-EXCEPT-YTD > 99999.00
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E018' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN MATURITY DATE IS PAST
      * FMZ -> KSL WHEN MATURITY DATE IS PAST
      * KSL -> NYK WHEN MATURITY DATE IS PAST
      * NYK -> DVP WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REGION-CD
                   ADD LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BATCH TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3260-FLAG-FLOAT.
           IF LBXM-FLOAT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3270-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBXM-EXCEPT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-EXCEPT-AMT - (WS-WORK-AMT * 3)
           COMPUTE LBXM-FLOAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3280-LIMIT-REMIT.
           IF LBXM-REMIT-AMT < 25.00
               MOVE 25.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-REMIT-AMT > 2500.00
               MOVE 2500.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FLOAT AMT.
      *----------------------------------------------------------------*
       3290-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-FLOAT-AMT ROUNDED =
                   LBXM-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXT-FLOAT-AMT
           END-IF
           ADD LBXT-FLOAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CHECK YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXM-CHECK-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-CHECK-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-REMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3310-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-DEPOSIT-YTD ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 5.00 TO LBXM-DEPOSIT-YTD
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 7.00 TO LBXM-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE LBXM-DEPOSIT-YTD ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-DEPOSIT-YTD > 0 AND LBXM-DEPOSIT-YTD < 2.00
               MOVE 2.00 TO LBXM-DEPOSIT-YTD
           END-IF
           IF LBXM-DEPOSIT-YTD > 250.00
               MOVE 250.00 TO LBXM-DEPOSIT-YTD
           END-IF
           ADD LBXM-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASH ON HAND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-CASH-ON-HAND.
           COMPUTE WS-WORK-AMT = LBXM-CASH-ON-HAND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = LBXM-CASH-ON-HAND-YTD - (WS-WORK-AMT
               * 4)
           COMPUTE LBXT-CASH-ON-HAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-REMIT-AMT ROUNDED =
                   LBXT-FLOAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-REMIT-AMT
           END-IF
           ADD LBXM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-EXCEPT.
           IF LBXM-EXCEPT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 10
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E461 MEMO REQUIRED
      * E602 WORK AMT NEGATIVE
      * E700 WORK AMT OVER 50000
      * E172 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN LBXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBXM-EXCEPT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-EXCEPT-YTD - (WS-WORK-AMT * 3)
           COMPUTE LBXM-FLOAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ON HAND YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CASH-ON-HAND-YTD * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-CHECK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3390-LIMIT-FEE.
           IF LBXM-FEE-AMT < 10.00
               MOVE 10.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-AMT > 500.00
               MOVE 500.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXM-DEPOSIT-AMT NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXM-REMIT-YTD NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBXM-DEPOSIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-DEPOSIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = LBXT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE LBXM-CHECK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3440-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD LBXM-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXM-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E272 ADDR REQUIRED
      * E942 REMIT YTD NEGATIVE
      * E712 REMIT YTD OVER 250000
      * E038 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-ADDR = SPACES
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-YTD < 0
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-YTD > 250000.00
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN LBXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REGION-CD
                   ADD LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CHECK-AMT ROUNDED =
                   LBXM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-CHECK-AMT
           END-IF
           ADD LBXT-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EPM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FZY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XUU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YBH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE CHECK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3520-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-HOLD-FLAG = 'Y'
                   COMPUTE LBXM-FEE-AMT ROUNDED = LBXT-CHECK-AMT *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-LOCKBOX-CD = 'DX'
                   MOVE 3.00 TO LBXM-FEE-AMT
               WHEN LBXM-LOCKBOX-CD = 'JM'
                   MOVE 7.00 TO LBXM-FEE-AMT
               WHEN OTHER
                   COMPUTE LBXM-FEE-AMT ROUNDED = LBXT-CHECK-AMT *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-FEE-AMT > 0 AND LBXM-FEE-AMT < 5.00
               MOVE 5.00 TO LBXM-FEE-AMT
           END-IF
           IF LBXM-FEE-AMT > 100.00
               MOVE 100.00 TO LBXM-FEE-AMT
           END-IF
           ADD LBXM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-SCHED-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-FEE-AMT NOT < WS-EXCEPT-SCHED-LOW(WS-TX)
                   MOVE WS-EXCEPT-SCHED-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-CASH-ON-HAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E943 CONTACT REQUIRED
      * E985 CHECK AMT NEGATIVE
      * E663 CHECK AMT OVER 50000
      * E167 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT < 0
                   MOVE 'E985' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT > 50000.00
                   MOVE 'E663' TO WS-REJECT-CODE
               WHEN LBXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHECK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXM-CHECK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-CHECK-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E680 CONTACT REQUIRED
      * E750 FLOAT YTD NEGATIVE
      * E348 FLOAT YTD OVER 50000
      * E564 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD < 0
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD > 50000.00
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN LBXM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXT-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3620-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-YTD > 2500.00
               MOVE 2500.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 6
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FLOAT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-FLOAT.
           COMPUTE WS-WORK-AMT = LBXM-FLOAT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-FLOAT-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-CHECK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 10
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD LBXM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE FLOAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXT-FLOAT-AMT NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN CLOSE DATE IS PAST
      * UK -> VR WHEN CLOSE DATE IS PAST
      * VR -> XG WHEN CLOSE DATE IS PAST
      * XG -> GF WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RC -> DP WHEN EFFECTIVE DATE IS PAST
      * DP -> EX WHEN EFFECTIVE DATE IS PAST
      * EX -> RJ WHEN EFFECTIVE DATE IS PAST
      * RJ -> AD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3710-LIMIT-FEE.
           IF LBXM-FEE-AMT < 50.00
               MOVE 50.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-AMT > 500.00
               MOVE 500.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E305 DESC REQUIRED
      * E940 REMIT AMT NEGATIVE
      * E749 REMIT AMT OVER 99999
      * E439 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-DESC = SPACES
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-AMT < 0
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-AMT > 99999.00
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3750-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXT-FEE-AMT ROUNDED = WS-WORK-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 7.00 TO LBXT-FEE-AMT
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 3.00 TO LBXT-FEE-AMT
               WHEN OTHER
                   COMPUTE LBXT-FEE-AMT ROUNDED = WS-WORK-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXT-FEE-AMT > 0 AND LBXT-FEE-AMT < 10.00
               MOVE 10.00 TO LBXT-FEE-AMT
           END-IF
           IF LBXT-FEE-AMT > 25.00
               MOVE 25.00 TO LBXT-FEE-AMT
           END-IF
           ADD LBXT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-DEPOSIT.
           IF LBXM-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-DEPOSIT-AMT * LBXM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E579 CUST ID REQUIRED
      * E881 FLOAT YTD NEGATIVE
      * E993 FLOAT YTD OVER 1000000
      * E587 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD < 0
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD > 1000000.00
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN LBXT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-FLOAT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE LBXM-FLOAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3790-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 3
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-FLOAT.
           IF LBXM-FLOAT-YTD < 0.00
               MOVE 0.00 TO LBXM-FLOAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FLOAT-YTD > 1000.00
               MOVE 1000.00 TO LBXM-FLOAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3820-FLAG-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-CASH-ON-HAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN NEXT DUE DATE IS PAST
      * UK -> VR WHEN NEXT DUE DATE IS PAST
      * VR -> XG WHEN NEXT DUE DATE IS PAST
      * XG -> GF WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 6
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-TRAN-CD
                   ADD LBXM-CHECK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT < 10.00
               MOVE 10.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 5000.00
               MOVE 5000.00 TO LBXT-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE FLOAT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3880-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-DEPOSIT-AMT ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 7.00 TO LBXM-DEPOSIT-AMT
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 10.00 TO LBXM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE LBXM-DEPOSIT-AMT ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-DEPOSIT-AMT > 0 AND LBXM-DEPOSIT-AMT < 2.00
               MOVE 2.00 TO LBXM-DEPOSIT-AMT
           END-IF
           IF LBXM-DEPOSIT-AMT > 100.00
               MOVE 100.00 TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3910-FLAG-FEE.
           IF LBXM-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-AMT ROUNDED =
                   LBXM-CASH-ON-HAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT GRID TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXM-REMIT-YTD NOT < WS-FLOAT-GRID-LOW(WS-TX)
                   MOVE WS-FLOAT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT SCHED TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-REMIT-AMT NOT < WS-EXCEPT-SCHED-LOW(WS-TX)
                   MOVE WS-EXCEPT-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-EXCEPT.
           IF LBXM-EXCEPT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-EXCEPT-YTD * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FLOAT AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-FLOAT-AMT ROUNDED =
                   LBXM-CHECK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-FLOAT-AMT
           END-IF
           ADD LBXT-FLOAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-FLOAT.
           IF LBXT-FLOAT-AMT < 25.00
               MOVE 25.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FLOAT-AMT > 10000.00
               MOVE 10000.00 TO LBXT-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       4030-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-YTD ROUNDED =
                   LBXM-FLOAT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-YTD
           END-IF
           ADD LBXM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4050-FLAG-FLOAT.
           IF LBXM-FLOAT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-FLOAT.
           IF LBXM-FLOAT-AMT < 0.00
               MOVE 0.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FLOAT-AMT > 10000.00
               MOVE 10000.00 TO LBXM-FLOAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-CHECK.
           IF LBXT-CHECK-AMT < 25.00
               MOVE 25.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CHECK-AMT > 5000.00
               MOVE 5000.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 5
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE CHECK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXT-CHECK-AMT NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHECK YTD FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4120-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CHECK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-STOP-FLAG = 'Y'
                   COMPUTE LBXM-CHECK-YTD ROUNDED = WS-WORK-AMT *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 7.00 TO LBXM-CHECK-YTD
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 5.00 TO LBXM-CHECK-YTD
               WHEN OTHER
                   COMPUTE LBXM-CHECK-YTD ROUNDED = WS-WORK-AMT *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-CHECK-YTD > 0 AND LBXM-CHECK-YTD < 10.00
               MOVE 10.00 TO LBXM-CHECK-YTD
           END-IF
           IF LBXM-CHECK-YTD > 75.00
               MOVE 75.00 TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4130-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-AMT < 10.00
               MOVE 10.00 TO LBXM-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-AMT > 500.00
               MOVE 500.00 TO LBXM-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE CASH ON HAND YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4150-ASSESS-STOP.
           EVALUATE TRUE
               WHEN LBXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO LBXM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-DEPOSIT-AMT ROUNDED =
                       LBXM-CASH-ON-HAND-YTD * LBXM-DISCOUNT / 200
               WHEN LBXT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO LBXM-DEPOSIT-AMT
               WHEN LBXT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO LBXM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE LBXM-DEPOSIT-AMT ROUNDED =
                       LBXM-CASH-ON-HAND-YTD * LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-DEPOSIT-AMT > 0 AND LBXM-DEPOSIT-AMT < 5.00
               MOVE 5.00 TO LBXM-DEPOSIT-AMT
           END-IF
           IF LBXM-DEPOSIT-AMT > 35.00
               MOVE 35.00 TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4160-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * LBXM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-EXCEPT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4170-LIMIT-CHECK.
           IF LBXT-CHECK-AMT < 10.00
               MOVE 10.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CHECK-AMT > 10000.00
               MOVE 10000.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4180-LIMIT-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-AMT < 100.00
               MOVE 100.00 TO LBXM-CASH-ON-HAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 500.00
               MOVE 500.00 TO LBXM-CASH-ON-HAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE FLOAT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4190-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-FLOAT-AMT * LBXM-DISCOUNT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 3.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 7.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-FLOAT-AMT * LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-AMT > 0 AND LBXM-CASH-ON-HAND-AMT < 1.00
               MOVE 1.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 75.00
               MOVE 75.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO LBXM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REMIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-REMIT.
           IF LBXM-REMIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-REMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-LBFEE.
           INITIALIZE LBFEE-PARMS
           MOVE LBXM-EXCEPT-YTD
             TO LBFEE-EXCEPT-YTD
           MOVE LBXM-DEPOSIT-YTD
             TO LBFEE-DEPOSIT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBFEE-REMIT-YTD
           MOVE LBXM-EXCEPT-AMT
             TO LBFEE-EXCEPT-AMT
           MOVE LBXM-FLOAT-YTD
             TO LBFEE-FLOAT-YTD
           MOVE LBXM-FLOAT-AMT
             TO LBFEE-FLOAT-AMT
           MOVE LBXM-CLOSE-DATE
             TO LBFEE-CLOSE-DATE
           MOVE LBXM-NEXT-DUE-DATE
             TO LBFEE-NEXT-DUE-DATE
           MOVE LBXM-LAST-PAY-DATE
             TO LBFEE-LAST-PAY-DATE
           MOVE LBXM-OPEN-DATE
             TO LBFEE-OPEN-DATE
           MOVE LBXM-LOCKBOX-CD
             TO LBFEE-LOCKBOX-CD
           MOVE LBXM-REASON-CD
             TO LBFEE-REASON-CD
           MOVE LBXM-BATCH-TYPE-CD
             TO LBFEE-BATCH-TYPE-CD
           MOVE LBXM-CHANNEL-CD
             TO LBFEE-CHANNEL-CD
           MOVE LBXM-STOP-FLAG
             TO LBFEE-STOP-FLAG
           MOVE LBXM-EXEMPT-FLAG
             TO LBFEE-EXEMPT-FLAG
           MOVE LBXM-NOTICE-SENT-FLAG
             TO LBFEE-NOTICE-SENT-FLAG
           MOVE LBXM-PAYMENT-CNT
             TO LBFEE-PAYMENT-CNT
           MOVE LBXM-ITEM-CNT
             TO LBFEE-ITEM-CNT
           MOVE LBXM-DISCOUNT
             TO LBFEE-DISCOUNT
           MOVE LBXM-PENALTY-PCT
             TO LBFEE-PENALTY-PCT
           MOVE LBXM-NAME
             TO LBFEE-NAME
           MOVE LBXM-ADDR
             TO LBFEE-ADDR
           CALL 'LBFEE' USING LBFEE-PARMS
           EVALUATE LBFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBFEE-EXCEPT-YTD
                     TO LBXM-EXCEPT-YTD
                   MOVE LBFEE-DEPOSIT-YTD
                     TO LBXM-DEPOSIT-YTD
                   MOVE LBFEE-STOP-FLAG
                     TO LBXM-STOP-FLAG
                   MOVE LBFEE-EXEMPT-FLAG
                     TO LBXM-EXEMPT-FLAG
                   MOVE LBFEE-NOTICE-SENT-FLAG
                     TO LBXM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-LBFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-LBWAIV.
           INITIALIZE LBWAIV-PARMS
           MOVE LBXM-EXCEPT-AMT
             TO LBWAIV-EXCEPT-AMT
           MOVE LBXM-FLOAT-AMT
             TO LBWAIV-FLOAT-AMT
           MOVE LBXM-CHECK-YTD
             TO LBWAIV-CHECK-YTD
           MOVE LBXM-CASH-ON-HAND-YTD
             TO LBWAIV-CASH-ON-HAND-YTD
           MOVE LBXM-FLOAT-YTD
             TO LBWAIV-FLOAT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBWAIV-REMIT-YTD
           MOVE LBXM-MATURITY-DATE
             TO LBWAIV-MATURITY-DATE
           MOVE LBXM-EXPIRY-DATE
             TO LBWAIV-EXPIRY-DATE
           MOVE LBXM-LAST-PAY-DATE
             TO LBWAIV-LAST-PAY-DATE
           MOVE LBXM-LAST-STMT-DATE
             TO LBWAIV-LAST-STMT-DATE
           MOVE LBXM-REASON-CD
             TO LBWAIV-REASON-CD
           MOVE LBXM-LOCKBOX-CD
             TO LBWAIV-LOCKBOX-CD
           MOVE LBXM-REGION-CD
             TO LBWAIV-REGION-CD
           MOVE LBXM-CHANNEL-CD
             TO LBWAIV-CHANNEL-CD
           MOVE LBXM-PREFERRED-FLAG
             TO LBWAIV-PREFERRED-FLAG
           MOVE LBXM-EXEMPT-FLAG
             TO LBWAIV-EXEMPT-FLAG
           MOVE LBXM-STOP-FLAG
             TO LBWAIV-STOP-FLAG
           MOVE LBXM-PAYMENT-CNT
             TO LBWAIV-PAYMENT-CNT
           MOVE LBXM-EXCEPT-CNT
             TO LBWAIV-EXCEPT-CNT
           MOVE LBXM-PENALTY-PCT
             TO LBWAIV-PENALTY-PCT
           MOVE LBXM-DISCOUNT
             TO LBWAIV-DISCOUNT
           MOVE LBXM-CONTACT
             TO LBWAIV-CONTACT
           MOVE LBXM-CUST-ID
             TO LBWAIV-CUST-ID
           CALL 'LBWAIV' USING LBWAIV-PARMS
           EVALUATE LBWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBWAIV-FLOAT-AMT
                     TO LBXM-FLOAT-AMT
                   MOVE LBWAIV-CASH-ON-HAND-YTD
                     TO LBXM-CASH-ON-HAND-YTD
                   MOVE LBWAIV-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBWAIV-REMIT-YTD
                     TO LBXM-REMIT-YTD
                   MOVE LBWAIV-EXEMPT-FLAG
                     TO LBXM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-LBWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-LBCAL.
           INITIALIZE LBCAL-PARMS
           MOVE LBXM-DEPOSIT-AMT
             TO LBCAL-DEPOSIT-AMT
           MOVE LBXM-DEPOSIT-YTD
             TO LBCAL-DEPOSIT-YTD
           MOVE LBXM-CHECK-AMT
             TO LBCAL-CHECK-AMT
           MOVE LBXM-CASH-ON-HAND-YTD
             TO LBCAL-CASH-ON-HAND-YTD
           MOVE LBXM-FLOAT-YTD
             TO LBCAL-FLOAT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBCAL-REMIT-YTD
           MOVE LBXM-NOTICE-DATE
             TO LBCAL-NOTICE-DATE
           MOVE LBXM-NEXT-DUE-DATE
             TO LBCAL-NEXT-DUE-DATE
           MOVE LBXM-OPEN-DATE
             TO LBCAL-OPEN-DATE
           MOVE LBXM-EFFECTIVE-DATE
             TO LBCAL-EFFECTIVE-DATE
           MOVE LBXM-REGION-CD
             TO LBCAL-REGION-CD
           MOVE LBXM-BATCH-TYPE-CD
             TO LBCAL-BATCH-TYPE-CD
           MOVE LBXM-REASON-CD
             TO LBCAL-REASON-CD
           MOVE LBXM-LOCKBOX-CD
             TO LBCAL-LOCKBOX-CD
           MOVE LBXM-HOLD-FLAG
             TO LBCAL-HOLD-FLAG
           MOVE LBXM-PREFERRED-FLAG
             TO LBCAL-PREFERRED-FLAG
           MOVE LBXM-NOTICE-SENT-FLAG
             TO LBCAL-NOTICE-SENT-FLAG
           MOVE LBXM-EXCEPT-CNT
             TO LBCAL-EXCEPT-CNT
           MOVE LBXM-RETRY-CNT
             TO LBCAL-RETRY-CNT
           MOVE LBXM-PENALTY-PCT
             TO LBCAL-PENALTY-PCT
           MOVE LBXM-DISCOUNT
             TO LBCAL-DISCOUNT
           MOVE LBXM-DESC
             TO LBCAL-DESC
           MOVE LBXM-CONTACT
             TO LBCAL-CONTACT
           CALL 'LBCAL' USING LBCAL-PARMS
           EVALUATE LBCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBCAL-DEPOSIT-AMT
                     TO LBXM-DEPOSIT-AMT
                   MOVE LBCAL-DEPOSIT-YTD
                     TO LBXM-DEPOSIT-YTD
                   MOVE LBCAL-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBCAL-PREFERRED-FLAG
                     TO LBXM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-LBCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE LBXT-KEY TO RD-KEY
           MOVE LBXT-TRAN-CD TO RD-CODE
           MOVE LBXT-FLOAT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE LBXT-KEY TO RD-KEY
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
           DISPLAY 'LBXCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'LBXCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
