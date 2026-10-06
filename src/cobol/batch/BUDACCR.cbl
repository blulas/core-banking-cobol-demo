      *================================================================*
      * PROGRAM  : BUDACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BUMP-UP CERTIFICATES - DAILY ACCRUAL. READS THE BCD *
      *            TRANSACTIONS, UPDATES THE BCD MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BUMP-UP AND STEP-RATE           *
      *            CERTIFICATE RATE ELECTION AND ACCRUAL               *
      * INPUT    : BCDMST (KSDS), BCDTRN, BANKCTL                      *
      * OUTPUT   : BCDOUT, BUDACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BUSTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-03-28 MKL  ORIGINAL                                       *
      * 1997-10-17 SKN  AUDIT FINDING - ADDON CONTROL TOTALS REQ4153   *
      * 1998-12-12 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-11-16 CRP  AUDIT FINDING - PENALTY CONTROL TOTALS CHG1664 *
      * 2004-12-08 MKL  INTEREST CUTOFF MOVED INC5304                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDACCR.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "BCDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BCDM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "BCDTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "BCDOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "BUDACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY BCDMST.
       FD  TRAN-FILE.
       COPY BCDTRN.
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
                                           VALUE 21.000000.
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
      * WS-BUMP-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUMP-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000038000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000129000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000819000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000102500'.
       01  WS-BUMP-SCHED-TBL REDEFINES WS-BUMP-SCHED-VALUES.
           05  WS-BUMP-SCHED-ROW OCCURS 4 TIMES.
               10  WS-BUMP-SCHED-LOW       PIC 9(09)V99.
               10  WS-BUMP-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000376000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000799500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000327000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000368000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000027000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000570000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023150000000757500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024150000000504000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034150000000854000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036650000000469500'.
       01  WS-PRINCIPA-TIER-TBL REDEFINES WS-PRINCIPA-TIER-VALUES.
           05  WS-PRINCIPA-TIER-ROW OCCURS 10 TIMES.
               10  WS-PRINCIPA-TIER-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BUDACCR  BUMP-UP CERTIFICATES'.
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
       COPY BUSTATP.

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
               MOVE 'BCDMST' TO ABND-FILE-NAME
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
           MOVE BCDT-KEY TO BCDM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SELECT-RATE
               PERFORM 3010-SET-REGION
               PERFORM 3020-LIMIT-BUMP
               PERFORM 3030-SPLIT-ADDON
               PERFORM 3040-SET-CHANNEL
               PERFORM 3050-DESCRIBE-CHANNEL
               PERFORM 3060-CHECK-LAST-PAY
               PERFORM 3070-COUNT-EXCEPT
               PERFORM 3080-EDIT-OPERATOR
               PERFORM 3090-COUNT-CYCLE
               PERFORM 3100-AGE-LAST-PAY
               PERFORM 3110-FLAG-BUMP
               PERFORM 3120-ACCRUE-ADDON
               PERFORM 3130-DESCRIBE-CYCLE
               PERFORM 3140-FLAG-ADDON
               PERFORM 3150-SET-CHANNEL
               PERFORM 3160-LIMIT-INTEREST
               PERFORM 3170-PRORATE-ADDON
               PERFORM 3180-AGE-NEXT-REVIEW
               PERFORM 3190-AGE-NEXT-REVIEW
               PERFORM 3200-SPLIT-INTEREST
               PERFORM 3210-SPLIT-INTEREST
               PERFORM 3220-SPLIT-PENALTY
               PERFORM 3230-FLAG-ADDON
               PERFORM 3240-PRORATE-PRINCIPAL
               PERFORM 3250-ASSESS-REVERSAL
               PERFORM 3260-FLAG-ITEM
               PERFORM 3270-PRORATE-BUMP
               PERFORM 3280-SET-TRAN
               PERFORM 3290-ACCRUE-BUMP
               PERFORM 3300-SET-SOURCE
               PERFORM 3310-SELECT-PENALTY-PCT
               PERFORM 3320-ACCRUE-BUMP
               PERFORM 3330-SET-CHANNEL
               PERFORM 3340-SET-CYCLE
               PERFORM 3350-SELECT-RATE
               PERFORM 3360-EDIT-CUST-ID
               PERFORM 3370-DESCRIBE-CHANNEL
               PERFORM 3380-AGE-EXPIRY
               PERFORM 3390-LIMIT-BUMP
               PERFORM 3400-AGE-LAST-MAINT
               PERFORM 3410-EDIT-MEMO
               PERFORM 3420-ACCUM-ELECTION
               PERFORM 3430-SET-STEP-PLAN
               PERFORM 3440-AGE-EXPIRY
               PERFORM 3450-LIMIT-BUMP
               PERFORM 3460-CHECK-EFF
               PERFORM 3470-ACCUM-TRAN
               PERFORM 3480-SPLIT-PRINCIPAL
               PERFORM 3490-SET-SOURCE
               PERFORM 3500-COUNT-EXCEPT
               PERFORM 3510-FLAG-BUMP
               PERFORM 3520-LIMIT-ADDON
               PERFORM 3530-EDIT-DESC
               PERFORM 3540-ACCUM-SOURCE
               PERFORM 3550-COUNT-EXCEPT
               PERFORM 3560-EDIT-OPERATOR
               PERFORM 3570-LIMIT-PRINCIPAL
               PERFORM 3580-DESCRIBE-TRAN
               PERFORM 3590-SET-REGION
               PERFORM 3600-PRORATE-PRINCIPAL
               PERFORM 3610-ACCUM-ELECTION
               PERFORM 3620-LIMIT-PENALTY
               PERFORM 3630-ACCUM-REGION
               PERFORM 3640-SPLIT-BUMP
               PERFORM 3650-PRORATE-BUMP
               PERFORM 3660-ACCRUE-BUMP
               PERFORM 3670-ACCRUE-INTEREST
               PERFORM 3680-ACCUM-REGION
               PERFORM 3690-COUNT-ITEM
               PERFORM 3700-ACCUM-CHANNEL
               PERFORM 3710-ACCRUE-BUMP
               PERFORM 3720-ACCRUE-PRINCIPAL
               PERFORM 3730-SPLIT-INTEREST
               PERFORM 3740-ACCRUE-BUMP
               PERFORM 3750-COUNT-EXCEPT
               PERFORM 3760-SELECT-MARGIN
               PERFORM 3770-ACCRUE-PENALTY
               PERFORM 3780-SPLIT-BUMP
               PERFORM 3790-SET-TRAN
               PERFORM 3800-FLAG-PENALTY
               PERFORM 3810-DESCRIBE-TRAN
               PERFORM 3820-COUNT-ITEM
               PERFORM 3830-CHECK-LAST-STMT
               PERFORM 3840-SPLIT-INTEREST
               PERFORM 3850-SET-ELECTION
               PERFORM 3860-EDIT-ADDR
               PERFORM 3870-DESCRIBE-CHANNEL
               PERFORM 3880-SELECT-RATE
               PERFORM 3890-EDIT-DESC
               PERFORM 3900-SELECT-PENALTY-PCT
               PERFORM 3910-SPLIT-ADDON
               PERFORM 3920-ASSESS-STOP
               PERFORM 3930-ACCUM-CYCLE
               PERFORM 3940-CHECK-NEXT-REVIEW
               PERFORM 3950-DESCRIBE-STEP-PLAN
               PERFORM 3960-SET-SOURCE
               PERFORM 3970-LIMIT-ADDON
               PERFORM 3980-DESCRIBE-STEP-PLAN
               PERFORM 3990-FLAG-BUMP
               PERFORM 4000-ACCUM-REGION
               PERFORM 4010-DESCRIBE-ELECTION
               PERFORM 4020-AGE-LAST-MAINT
               PERFORM 4030-ACCUM-CHANNEL
               PERFORM 4040-EDIT-REJECT
               PERFORM 4050-DESCRIBE-REGION
               PERFORM 4060-CHECK-EFF
               PERFORM 4070-EDIT-CUST-ID
               PERFORM 4080-FLAG-PENALTY
               PERFORM 4090-CHECK-LAST-PAY
               PERFORM 4100-COUNT-RETRY
               PERFORM 4110-CHECK-NEXT-DUE
               PERFORM 4120-SPLIT-BUMP
               PERFORM 4130-CHECK-CLOSE
               PERFORM 4140-SELECT-MARGIN
               PERFORM 4150-DESCRIBE-CHANNEL
               PERFORM 4160-AGE-LAST-STMT
               PERFORM 4170-ACCUM-CHANNEL
               PERFORM 4180-DESCRIBE-CHANNEL
               PERFORM 4190-SPLIT-BUMP
               PERFORM 4200-SELECT-PENALTY-PCT
               PERFORM 4210-CHECK-EFFECTIVE
               PERFORM 4220-COUNT-SEQ
               PERFORM 4230-AGE-LAST-MAINT
               PERFORM 4240-COUNT-ITEM
               PERFORM 4250-EDIT-ADDR
               PERFORM 4260-ACCUM-CHANNEL
               PERFORM 7030-CALL-BUSTAT
               REWRITE BCD-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'BCDMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM BCD-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA TIER TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-TIER-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BCDM-PRINCIPAL-AMT NOT < WS-PRINCIPA-TIER-LOW(WS-TX)
                   MOVE WS-PRINCIPA-TIER-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN LAST MAINT DATE IS PAST
      * LFX -> MLA WHEN LAST MAINT DATE IS PAST
      * MLA -> ULA WHEN LAST MAINT DATE IS PAST
      * ULA -> BGJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUMP YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3020-LIMIT-BUMP.
           IF BCDM-BUMP-YTD < 25.00
               MOVE 25.00 TO BCDM-BUMP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-BUMP-YTD > 10000.00
               MOVE 10000.00 TO BCDM-BUMP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDT-ADDON-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDT-ADDON-AMT - (WS-WORK-AMT * 12)
           COMPUTE BCDM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN CLOSE DATE IS PAST
      * JE -> PN WHEN CLOSE DATE IS PAST
      * PN -> WV WHEN CLOSE DATE IS PAST
      * WV -> EK WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EK' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E971 OPERATOR REQUIRED
      * E356 PRINCIPAL AMT NEGATIVE
      * E430 PRINCIPAL AMT OVER 50000
      * E320 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E971' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT < 0
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT > 50000.00
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E320' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUMP YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3110-FLAG-BUMP.
           IF BCDM-BUMP-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-BUMP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-ADDON.
           IF BCDM-ADDON-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-ADDON-YTD * BCDM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BCDM-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3140-FLAG-ADDON.
           IF BCDM-ADDON-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-ADDON-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-INTEREST.
           IF BCDM-INTEREST-AMT < 0.00
               MOVE 0.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-AMT > 50000.00
               MOVE 50000.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-ADDON-AMT ROUNDED =
                   BCDM-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0 AND BCDM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0 AND BCDM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-BUMP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT * 2)
           COMPUTE BCDT-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = BCDM-PENALTY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-PENALTY-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3230-FLAG-ADDON.
           IF BCDM-ADDON-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-ADDON-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                   BCDM-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3250-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDT-INTEREST-AMT
                       * BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 20.00 TO BCDM-PENALTY-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 15.00 TO BCDM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDT-INTEREST-AMT
                       * BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PENALTY-AMT > 0 AND BCDM-PENALTY-AMT < 5.00
               MOVE 5.00 TO BCDM-PENALTY-AMT
           END-IF
           IF BCDM-PENALTY-AMT > 150.00
               MOVE 150.00 TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3260-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-BUMP-AMT ROUNDED =
                   BCDM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDT-BUMP-AMT
           END-IF
           ADD BCDT-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN POST DATE IS PAST
      * AD -> WD WHEN POST DATE IS PAST
      * WD -> MT WHEN POST DATE IS PAST
      * MT -> BU WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-BUMP.
           IF BCDT-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-BUMP-AMT * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN LAST PAY DATE IS PAST
      * BF -> HF WHEN LAST PAY DATE IS PAST
      * HF -> HR WHEN LAST PAY DATE IS PAST
      * HR -> UF WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA TIER TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-TIER-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BCDT-ADDON-AMT NOT < WS-PRINCIPA-TIER-LOW(WS-TX)
                   MOVE WS-PRINCIPA-TIER-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-BUMP.
           IF BCDM-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-BUMP-AMT * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN EXPIRY DATE IS PAST
      * S -> T WHEN EXPIRY DATE IS PAST
      * T -> U WHEN EXPIRY DATE IS PAST
      * U -> X WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUMP SCHED TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BCDM-INTEREST-YTD NOT < WS-BUMP-SCHED-LOW(WS-TX)
                   MOVE WS-BUMP-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E720 CUST ID REQUIRED
      * E201 BUMP AMT NEGATIVE
      * E617 BUMP AMT OVER 5000
      * E865 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E720' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-AMT < 0
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-AMT > 5000.00
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUMP AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3390-LIMIT-BUMP.
           IF BCDT-BUMP-AMT < 100.00
               MOVE 100.00 TO BCDT-BUMP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-BUMP-AMT > 10000.00
               MOVE 10000.00 TO BCDT-BUMP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E108 MEMO REQUIRED
      * E649 PRINCIPAL YTD NEGATIVE
      * E377 PRINCIPAL YTD OVER 5000
      * E037 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-MEMO = SPACES
                   MOVE 'E108' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-YTD < 0
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-YTD > 5000.00
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN BCDM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-ELECTION-CD
                   ADD BCDT-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN NEXT REVIEW DATE IS PAST
      * DGU -> GWJ WHEN NEXT REVIEW DATE IS PAST
      * GWJ -> RJL WHEN NEXT REVIEW DATE IS PAST
      * RJL -> UPM WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUMP YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3450-LIMIT-BUMP.
           IF BCDM-BUMP-YTD < 50.00
               MOVE 50.00 TO BCDM-BUMP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-BUMP-YTD > 2500.00
               MOVE 2500.00 TO BCDM-BUMP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-TRAN-CD
                   ADD BCDT-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE BCDM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN LAST PAY DATE IS PAST
      * BF -> HF WHEN LAST PAY DATE IS PAST
      * HF -> HR WHEN LAST PAY DATE IS PAST
      * HR -> UF WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-BUMP.
           IF BCDM-BUMP-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3520-LIMIT-ADDON.
           IF BCDT-ADDON-AMT < 25.00
               MOVE 25.00 TO BCDT-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-ADDON-AMT > 2500.00
               MOVE 2500.00 TO BCDT-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E211 DESC REQUIRED
      * E145 BUMP AMT NEGATIVE
      * E859 BUMP AMT OVER 50000
      * E945 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT < 0
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT > 50000.00
                   MOVE 'E859' TO WS-REJECT-CODE
               WHEN BCDM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD BCDT-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E512 OPERATOR REQUIRED
      * E512 PENALTY AMT NEGATIVE
      * E066 PENALTY AMT OVER 99999
      * E058 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3560-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT < 0
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT > 99999.00
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3570-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD < 100.00
               MOVE 100.00 TO BCDM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-YTD > 5000.00
               MOVE 5000.00 TO BCDM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN LAST PAY DATE IS PAST
      * LFX -> MLA WHEN LAST PAY DATE IS PAST
      * MLA -> ULA WHEN LAST PAY DATE IS PAST
      * ULA -> BGJ WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3600-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                   BCDM-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-ELECTION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-PENALTY.
           IF BCDM-PENALTY-AMT < 10.00
               MOVE 10.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-AMT > 1000.00
               MOVE 1000.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-YTD - (WS-WORK-AMT * 3)
           COMPUTE BCDM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3650-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BCDM-BUMP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-BUMP.
           IF BCDM-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-BUMP-AMT * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-INTEREST.
           IF BCDM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-AMT * BCDM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BCDM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-CHANNEL-CD
                   ADD BCDT-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-BUMP.
           IF BCDM-BUMP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-BUMP-YTD * BCDM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PRINCIPAL-AMT * BCDM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BCDT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-AMT - (WS-WORK-AMT * 2)
           COMPUTE BCDM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-BUMP.
           IF BCDM-BUMP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-BUMP-YTD * BCDM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BCDM-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-BUMP-SCHED-LOW(WS-TX)
                   MOVE WS-BUMP-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-PENALTY.
           IF BCDT-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-PENALTY-AMT * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDM-ADDON-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-AMT - (WS-WORK-AMT * 2)
           COMPUTE BCDT-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN NEXT REVIEW DATE IS PAST
      * AD -> WD WHEN NEXT REVIEW DATE IS PAST
      * WD -> MT WHEN NEXT REVIEW DATE IS PAST
      * MT -> BU WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3800-FLAG-PENALTY.
           IF BCDM-PENALTY-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BCDM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-YTD - (WS-WORK-AMT * 2)
           COMPUTE BCDM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> R WHEN EXPIRY DATE IS PAST
      * R -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E432 ADDR REQUIRED
      * E453 BUMP AMT NEGATIVE
      * E207 BUMP AMT OVER 50000
      * E632 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-ADDR = SPACES
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT < 0
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT > 50000.00
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN BCDM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E632' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUMP SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-SCHED-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-BUMP-SCHED-LOW(WS-TX)
                   MOVE WS-BUMP-SCHED-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E570 DESC REQUIRED
      * E133 PENALTY AMT NEGATIVE
      * E652 PENALTY AMT OVER 99999
      * E257 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E570' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT < 0
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT > 99999.00
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA TIER TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3900-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-TIER-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BCDT-INTEREST-AMT NOT < WS-PRINCIPA-TIER-LOW(WS-TX)
                   MOVE WS-PRINCIPA-TIER-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDT-ADDON-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BCDT-ADDON-AMT - (WS-WORK-AMT * 4)
           COMPUTE BCDM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE BUMP AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3920-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO BCDT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDT-BUMP-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO BCDT-ADDON-AMT
               WHEN BCDT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO BCDT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDT-BUMP-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDT-ADDON-AMT > 0 AND BCDT-ADDON-AMT < 10.00
               MOVE 10.00 TO BCDT-ADDON-AMT
           END-IF
           IF BCDT-ADDON-AMT > 35.00
               MOVE 35.00 TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN POST DATE IS PAST
      * BF -> HF WHEN POST DATE IS PAST
      * HF -> HR WHEN POST DATE IS PAST
      * HR -> UF WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3970-LIMIT-ADDON.
           IF BCDM-ADDON-YTD < 100.00
               MOVE 100.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-YTD > 1000.00
               MOVE 1000.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3990-FLAG-BUMP.
           IF BCDM-BUMP-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CHANNEL-CD
                   ADD BCDM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E979 REJECT CD REQUIRED
      * E819 INTEREST AMT NEGATIVE
      * E736 INTEREST AMT OVER 5000
      * E463 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-REJECT-CD = SPACES
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-AMT < 0
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-AMT > 5000.00
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E108 CUST ID REQUIRED
      * E161 PRINCIPAL YTD NEGATIVE
      * E908 PRINCIPAL YTD OVER 25000
      * E540 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E108' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-YTD < 0
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-YTD > 25000.00
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4080-FLAG-PENALTY.
           IF BCDM-PENALTY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 12
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDT-BUMP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-BUMP-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BCDM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP SCHED TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BCDM-ADDON-AMT NOT < WS-BUMP-SCHED-LOW(WS-TX)
                   MOVE WS-BUMP-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CHANNEL-CD
                   ADD BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDT-BUMP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-BUMP-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP SCHED TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-SCHED-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BCDM-ADDON-AMT NOT < WS-BUMP-SCHED-LOW(WS-TX)
                   MOVE WS-BUMP-SCHED-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 12
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 12
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E679 ADDR REQUIRED
      * E560 PENALTY YTD NEGATIVE
      * E563 PENALTY YTD OVER 99999
      * E822 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-ADDR = SPACES
                   MOVE 'E679' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-YTD < 0
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-YTD > 99999.00
                   MOVE 'E563' TO WS-REJECT-CODE
               WHEN BCDM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-CHANNEL-CD
                   ADD BCDM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BUSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-BUSTAT.
           INITIALIZE BUSTAT-PARMS
           MOVE BCDM-PENALTY-YTD
             TO BUSTAT-PENALTY-YTD
           MOVE BCDM-ADDON-AMT
             TO BUSTAT-ADDON-AMT
           MOVE BCDM-BUMP-AMT
             TO BUSTAT-BUMP-AMT
           MOVE BCDM-PENALTY-AMT
             TO BUSTAT-PENALTY-AMT
           MOVE BCDM-PRINCIPAL-AMT
             TO BUSTAT-PRINCIPAL-AMT
           MOVE BCDM-INTEREST-YTD
             TO BUSTAT-INTEREST-YTD
           MOVE BCDM-LAST-PAY-DATE
             TO BUSTAT-LAST-PAY-DATE
           MOVE BCDM-EXPIRY-DATE
             TO BUSTAT-EXPIRY-DATE
           MOVE BCDM-MATURITY-DATE
             TO BUSTAT-MATURITY-DATE
           MOVE BCDM-LAST-STMT-DATE
             TO BUSTAT-LAST-STMT-DATE
           MOVE BCDM-ELECTION-CD
             TO BUSTAT-ELECTION-CD
           MOVE BCDM-CHANNEL-CD
             TO BUSTAT-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUSTAT-SOURCE-CD
           MOVE BCDM-STEP-PLAN-CD
             TO BUSTAT-STEP-PLAN-CD
           MOVE BCDM-OVERRIDE-FLAG
             TO BUSTAT-OVERRIDE-FLAG
           MOVE BCDM-REVIEW-FLAG
             TO BUSTAT-REVIEW-FLAG
           MOVE BCDM-STOP-FLAG
             TO BUSTAT-STOP-FLAG
           MOVE BCDM-CYCLE-CNT
             TO BUSTAT-CYCLE-CNT
           MOVE BCDM-ITEM-CNT
             TO BUSTAT-ITEM-CNT
           MOVE BCDM-PENALTY-PCT
             TO BUSTAT-PENALTY-PCT
           MOVE BCDM-RATE
             TO BUSTAT-RATE
           MOVE BCDM-ADDR
             TO BUSTAT-ADDR
           MOVE BCDM-NAME
             TO BUSTAT-NAME
           CALL 'BUSTAT' USING BUSTAT-PARMS
           EVALUATE BUSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BUSTAT-BUMP-AMT
                     TO BCDM-BUMP-AMT
                   MOVE BUSTAT-PENALTY-AMT
                     TO BCDM-PENALTY-AMT
                   MOVE BUSTAT-REVIEW-FLAG
                     TO BCDM-REVIEW-FLAG
                   MOVE BUSTAT-STOP-FLAG
                     TO BCDM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-BUSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE BCDT-KEY TO RD-KEY
           MOVE BCDT-TRAN-CD TO RD-CODE
           MOVE BCDT-INTEREST-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE BCDT-KEY TO RD-KEY
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
           DISPLAY 'BUDACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BUDACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
