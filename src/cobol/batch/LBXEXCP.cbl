      *================================================================*
      * PROGRAM  : LBXEXCP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : LOCKBOX REMITTANCE - EXCEPTION PROCESSING. READS THE*
      *            LBX TRANSACTIONS, UPDATES THE LBX MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. WHOLESALE LOCKBOX      *
      *            REMITTANCE CAPTURE AND CREDIT TO CLIENTS            *
      * INPUT    : LBXMST (KSDS), LBXTRN, BANKCTL                      *
      * OUTPUT   : LBXOUT, LBXEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, LBFEE, LBCAL, LBWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-01 TWH  ORIGINAL                                       *
      * 2000-03-02 GAF  CHECK NOTICE TEXT PER LEGAL REQ1530            *
      * 2001-08-18 EWS  FLOAT TABLE EXPANDED PRB5111                   *
      * 2003-07-07 DPS  REMIT CUTOFF MOVED REQ5802                     *
      * 2003-06-21 DPS  FLOAT STATUS CODES ADDED CHG0306               *
      * 2005-07-20 DPS  CASH-ON-HAND TABLE EXPANDED INC9086            *
      * 2006-02-25 TWH  FLOAT STATUS CODES ADDED PRB6108               *
      * 2009-11-12 CRP  FEE STATUS CODES ADDED CHG1792                 *
      * 2011-01-14 JLM  CASH-ON-HAND FEE SCHEDULE UPDATE PRB8105       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBXEXCP.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "LBXEXRPT"
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
                                           VALUE 9.000000.
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
      * WS-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000599000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000078000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000466000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000377500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020350000000679000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022850000000831500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023100000000458000'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 7 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEPOSIT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000591500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000726000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000171000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000668500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000291000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004550000000484000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009550000000066500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019550000000469500'.
       01  WS-DEPOSIT-GRID-TBL REDEFINES WS-DEPOSIT-GRID-VALUES.
           05  WS-DEPOSIT-GRID-ROW OCCURS 9 TIMES.
               10  WS-DEPOSIT-GRID-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000853000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000762500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000169000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000428000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000612000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010850000000245500'.
       01  WS-FEE-SCHEDX-TBL REDEFINES WS-FEE-SCHEDX-VALUES.
           05  WS-FEE-SCHEDX-ROW OCCURS 6 TIMES.
               10  WS-FEE-SCHEDX-LOW       PIC 9(09)V99.
               10  WS-FEE-SCHEDX-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  LBXEXCP  LOCKBOX REMITTANCE'.
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
       COPY LBCALP.
       COPY LBWAIVP.

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
               PERFORM 3000-FLAG-EXCEPT
               PERFORM 3010-DESCRIBE-REASON
               PERFORM 3020-SPLIT-DEPOSIT
               PERFORM 3030-ACCUM-CHANNEL
               PERFORM 3040-PRORATE-CHECK
               PERFORM 3050-SET-CHANNEL
               PERFORM 3060-PRORATE-CHECK
               PERFORM 3070-ACCUM-CHANNEL
               PERFORM 3080-ASSESS-HOLD
               PERFORM 3090-EDIT-REJECT
               PERFORM 3100-CHECK-EXPIRY
               PERFORM 3110-SET-CHANNEL
               PERFORM 3120-SET-BATCH-TYPE
               PERFORM 3130-EDIT-CUST-ID
               PERFORM 3140-LIMIT-DEPOSIT
               PERFORM 3150-EDIT-DESC
               PERFORM 3160-CHECK-LAST-STMT
               PERFORM 3170-LIMIT-FEE
               PERFORM 3180-FLAG-CHECK
               PERFORM 3190-ACCUM-BATCH-TYPE
               PERFORM 3200-EDIT-NAME
               PERFORM 3210-ACCRUE-FLOAT
               PERFORM 3220-ACCRUE-FEE
               PERFORM 3230-LIMIT-CASH-ON-HAND
               PERFORM 3240-SET-BATCH-TYPE
               PERFORM 3250-ACCRUE-DISCOUNT
               PERFORM 3260-LIMIT-ITEM
               PERFORM 3270-SELECT-PENALTY-PCT
               PERFORM 3280-COUNT-RETRY
               PERFORM 3290-ACCRUE-FEE
               PERFORM 3300-LIMIT-FEE
               PERFORM 3310-PRORATE-CASH-ON-HAND
               PERFORM 3320-COUNT-RETRY
               PERFORM 3330-ASSESS-REVERSAL
               PERFORM 3340-LIMIT-REMIT
               PERFORM 3350-SELECT-FEE-PCT
               PERFORM 3360-DESCRIBE-REGION
               PERFORM 3370-ASSESS-NOTICE-SENT
               PERFORM 3380-ACCRUE-DEPOSIT
               PERFORM 3390-COUNT-RETRY
               PERFORM 3400-COUNT-SEQ
               PERFORM 3410-AGE-LAST-PAY
               PERFORM 3420-AGE-LAST-STMT
               PERFORM 3430-SET-CHANNEL
               PERFORM 3440-SELECT-FEE-PCT
               PERFORM 3450-SPLIT-CASH-ON-HAND
               PERFORM 3460-ACCUM-REASON
               PERFORM 3470-SPLIT-REMIT
               PERFORM 3480-FLAG-FLOAT
               PERFORM 3490-ACCUM-LOCKBOX
               PERFORM 3500-DESCRIBE-CYCLE
               PERFORM 3510-SELECT-DISCOUNT
               PERFORM 3520-PRORATE-FEE
               PERFORM 3530-ACCRUE-EXCEPT
               PERFORM 3540-SPLIT-FEE
               PERFORM 3550-DESCRIBE-TRAN
               PERFORM 3560-SELECT-DISCOUNT
               PERFORM 3570-SET-REGION
               PERFORM 3580-SELECT-PENALTY-PCT
               PERFORM 3590-COUNT-EXCEPT
               PERFORM 3600-EDIT-CUST-ID
               PERFORM 3610-PRORATE-DEPOSIT
               PERFORM 3620-PRORATE-CHECK
               PERFORM 3630-CHECK-POST
               PERFORM 3640-AGE-NEXT-DUE
               PERFORM 3650-ACCRUE-FEE
               PERFORM 3660-PRORATE-FLOAT
               PERFORM 3670-PRORATE-CHECK
               PERFORM 3680-AGE-EFFECTIVE
               PERFORM 3690-SELECT-DISCOUNT
               PERFORM 3700-EDIT-ADDR
               PERFORM 3710-ACCRUE-FLOAT
               PERFORM 3720-AGE-LAST-PAY
               PERFORM 3730-SELECT-DISCOUNT
               PERFORM 3740-FLAG-ITEM
               PERFORM 3750-SELECT-PENALTY-PCT
               PERFORM 3760-EDIT-REF-NO
               PERFORM 3770-SET-REGION
               PERFORM 3780-COUNT-SEQ
               PERFORM 3790-ACCUM-TRAN
               PERFORM 3800-AGE-MATURITY
               PERFORM 3810-ASSESS-REVERSAL
               PERFORM 3820-ACCUM-REASON
               PERFORM 3830-EDIT-NAME
               PERFORM 3840-LIMIT-FEE
               PERFORM 3850-FLAG-FLOAT
               PERFORM 3860-CHECK-MATURITY
               PERFORM 3870-FLAG-DEPOSIT
               PERFORM 3880-SET-BATCH-TYPE
               PERFORM 3890-SET-REASON
               PERFORM 3900-SPLIT-CHECK
               PERFORM 3910-CHECK-LAST-STMT
               PERFORM 3920-ACCRUE-DEPOSIT
               PERFORM 3930-SELECT-FEE-PCT
               PERFORM 3940-SPLIT-FEE
               PERFORM 3950-COUNT-PAYMENT
               PERFORM 3960-COUNT-ITEM
               PERFORM 3970-ACCUM-CYCLE
               PERFORM 3980-FLAG-ITEM
               PERFORM 3990-DESCRIBE-TRAN
               PERFORM 4000-SPLIT-DEPOSIT
               PERFORM 4010-AGE-OPEN
               PERFORM 4020-SET-BATCH-TYPE
               PERFORM 4030-COUNT-RETRY
               PERFORM 4040-ASSESS-AUTO-RENEW
               PERFORM 4050-SET-LOCKBOX
               PERFORM 4060-ACCUM-CYCLE
               PERFORM 4070-ACCUM-REASON
               PERFORM 4080-SET-TRAN
               PERFORM 4090-AGE-POST
               PERFORM 4100-AGE-NOTICE
               PERFORM 4110-SET-REGION
               PERFORM 4120-PRORATE-DEPOSIT
               PERFORM 4130-ACCRUE-FEE
               PERFORM 4140-ASSESS-HOLD
               PERFORM 4150-SELECT-FEE-PCT
               PERFORM 4160-COUNT-EXCEPT
               PERFORM 4170-EDIT-CONTACT
               PERFORM 4180-COUNT-ITEM
               PERFORM 4190-SET-CHANNEL
               PERFORM 4200-ASSESS-HOLD
               PERFORM 4210-EDIT-REF-NO
               PERFORM 4220-ACCUM-LOCKBOX
               PERFORM 4230-ACCRUE-FEE
               PERFORM 7020-CALL-LBFEE
               PERFORM 7010-CALL-LBCAL
               PERFORM 7000-CALL-LBWAIV
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
      * EXCEPT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3000-FLAG-EXCEPT.
           IF LBXM-EXCEPT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-EXCEPT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBXM-DEPOSIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-DEPOSIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXM-CHECK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD LBXM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHECK YTD.
      *----------------------------------------------------------------*
       3040-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-YTD ROUNDED =
                   LBXM-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHECK YTD.
      *----------------------------------------------------------------*
       3060-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-YTD ROUNDED =
                   LBXM-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3080-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-DEPOSIT-YTD * LBXM-DISCOUNT / 200
               WHEN LBXT-TRAN-CD = 'RC'
                   MOVE 10.00 TO LBXT-CASH-ON-HAND-AMT
               WHEN LBXT-TRAN-CD = 'DP'
                   MOVE 15.00 TO LBXT-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-DEPOSIT-YTD * LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXT-CASH-ON-HAND-AMT > 0 AND LBXT-CASH-ON-HAND-AMT < 1.00
               MOVE 1.00 TO LBXT-CASH-ON-HAND-AMT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 100.00
               MOVE 100.00 TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E740 REJECT CD REQUIRED
      * E847 CASH ON HAND AMT NEGATIVE
      * E763 CASH ON HAND AMT OVER 10000
      * E757 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-REJECT-CD = SPACES
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-AMT < 0
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-AMT > 10000.00
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN EXPIRY DATE IS PAST
      * H -> Z WHEN EXPIRY DATE IS PAST
      * Z -> B WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN EXPIRY DATE IS PAST
      * D -> F WHEN EXPIRY DATE IS PAST
      * F -> K WHEN EXPIRY DATE IS PAST
      * K -> U WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E164 CUST ID REQUIRED
      * E008 CASH ON HAND YTD NEGATIVE
      * E471 CASH ON HAND YTD OVER 1000000
      * E537 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-YTD < 0
                   MOVE 'E008' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-YTD > 1000000.00
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN LBXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3140-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-AMT < 25.00
               MOVE 25.00 TO LBXM-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-AMT > 10000.00
               MOVE 10000.00 TO LBXM-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E911 DESC REQUIRED
      * E086 DEPOSIT YTD NEGATIVE
      * E997 DEPOSIT YTD OVER 25000
      * E070 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3150-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-DESC = SPACES
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD < 0
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD > 25000.00
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-LAST-STMT.
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
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-FEE.
           IF LBXT-FEE-AMT < 50.00
               MOVE 50.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FEE-AMT > 5000.00
               MOVE 5000.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3180-FLAG-CHECK.
           IF LBXT-CHECK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXT-CHECK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY BATCH TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-BATCH-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-BATCH-TYPE-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-BATCH-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E033 NAME REQUIRED
      * E353 CHECK YTD NEGATIVE
      * E194 CHECK YTD OVER 1000000
      * E230 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-NAME = SPACES
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-YTD < 0
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-YTD > 1000000.00
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN LBXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-FLOAT.
           IF LBXM-FLOAT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-YTD * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXT-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-FEE.
           IF LBXM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FEE-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH ON HAND YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD < 0.00
               MOVE 0.00 TO LBXM-CASH-ON-HAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-CASH-ON-HAND-YTD > 1000.00
               MOVE 1000.00 TO LBXM-CASH-ON-HAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN EFF DATE IS PAST
      * D -> F WHEN EFF DATE IS PAST
      * F -> K WHEN EFF DATE IS PAST
      * K -> U WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXT-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
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
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 10
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-FEE.
           IF LBXM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FEE-YTD * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3300-LIMIT-FEE.
           IF LBXT-FEE-AMT < 100.00
               MOVE 100.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FEE-AMT > 2500.00
               MOVE 2500.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ON HAND AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                   LBXM-DEPOSIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 12
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT YTD FEE ON THE CHECK YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3330-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-REMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-HOLD-FLAG = 'Y'
                   COMPUTE LBXM-REMIT-YTD ROUNDED = LBXM-CHECK-YTD *
                       LBXM-FEE-PCT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 20.00 TO LBXM-REMIT-YTD
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 15.00 TO LBXM-REMIT-YTD
               WHEN OTHER
                   COMPUTE LBXM-REMIT-YTD ROUNDED = LBXM-CHECK-YTD *
                       LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-REMIT-YTD > 0 AND LBXM-REMIT-YTD < 5.00
               MOVE 5.00 TO LBXM-REMIT-YTD
           END-IF
           IF LBXM-REMIT-YTD > 75.00
               MOVE 75.00 TO LBXM-REMIT-YTD
           END-IF
           ADD LBXM-REMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3340-LIMIT-REMIT.
           IF LBXM-REMIT-AMT < 50.00
               MOVE 50.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-REMIT-AMT > 500.00
               MOVE 500.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXM-DEPOSIT-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CHECK FEE ON THE EXCEPT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3370-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO LBXT-CHECK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE LBXT-CHECK-AMT ROUNDED = LBXM-EXCEPT-YTD *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-LOCKBOX-CD = 'DX'
                   MOVE 7.00 TO LBXT-CHECK-AMT
               WHEN LBXM-LOCKBOX-CD = 'JM'
                   MOVE 3.00 TO LBXT-CHECK-AMT
               WHEN OTHER
                   COMPUTE LBXT-CHECK-AMT ROUNDED = LBXM-EXCEPT-YTD *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXT-CHECK-AMT > 0 AND LBXT-CHECK-AMT < 2.00
               MOVE 2.00 TO LBXT-CHECK-AMT
           END-IF
           IF LBXT-CHECK-AMT > 250.00
               MOVE 250.00 TO LBXT-CHECK-AMT
           END-IF
           ADD LBXT-CHECK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-DEPOSIT.
           IF LBXM-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-DEPOSIT-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-DEPOSIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 10
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 12
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHEDX TIER FOR THE FLOAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHEDX-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXT-FLOAT-AMT NOT < WS-FEE-SCHEDX-LOW(WS-TX)
                   MOVE WS-FEE-SCHEDX-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ON HAND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-CASH-ON-HAND.
           COMPUTE WS-WORK-AMT = LBXM-CASH-ON-HAND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = LBXM-CASH-ON-HAND-AMT - (WS-WORK-AMT
               * 4)
           COMPUTE LBXT-CHECK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND YTD BY REASON CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXM-CHECK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3480-FLAG-FLOAT.
           IF LBXM-FLOAT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK YTD BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-LOCKBOX-CD
                   ADD LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EPM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FZY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XUU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YBH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE CHECK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF LBXM-CHECK-AMT NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3520-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-FEE-AMT ROUNDED =
                   LBXM-CHECK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-FEE-AMT
           END-IF
           ADD LBXM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-EXCEPT.
           IF LBXM-EXCEPT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-EXCEPT-AMT * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXT-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = LBXT-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE LBXM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT GRID TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF LBXM-REMIT-YTD NOT < WS-DEPOSIT-GRID-LOW(WS-TX)
                   MOVE WS-DEPOSIT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN EFFECTIVE DATE IS PAST
      * FMZ -> KSL WHEN EFFECTIVE DATE IS PAST
      * KSL -> NYK WHEN EFFECTIVE DATE IS PAST
      * NYK -> DVP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHEDX TIER FOR THE CHECK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHEDX-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXT-CHECK-AMT NOT < WS-FEE-SCHEDX-LOW(WS-TX)
                   MOVE WS-FEE-SCHEDX-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 CUST ID REQUIRED
      * E747 CHECK AMT NEGATIVE
      * E861 CHECK AMT OVER 10000
      * E268 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT < 0
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT > 10000.00
                   MOVE 'E861' TO WS-REJECT-CODE
               WHEN LBXM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       3610-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-YTD ROUNDED =
                   LBXM-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-YTD
           END-IF
           ADD LBXM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-AMT ROUNDED =
                   LBXM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-CHECK-AMT
           END-IF
           ADD LBXM-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-NEXT-DUE.
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
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-FEE.
           IF LBXM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FEE-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FLOAT YTD.
      *----------------------------------------------------------------*
       3660-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-FLOAT-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-FLOAT-YTD
           END-IF
           ADD LBXM-FLOAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-AMT ROUNDED =
                   LBXT-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CHECK-AMT
           END-IF
           ADD LBXM-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EFFECTIVE.
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
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXM-REMIT-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E491 ADDR REQUIRED
      * E248 WORK AMT 2 NEGATIVE
      * E886 WORK AMT 2 OVER 99999
      * E786 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-ADDR = SPACES
                   MOVE 'E491' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E886' TO WS-REJECT-CODE
               WHEN LBXM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-FLOAT.
           IF LBXM-FLOAT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-YTD * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHEDX TIER FOR THE CASH ON HAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHEDX-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXT-CASH-ON-HAND-AMT NOT < WS-FEE-SCHEDX-LOW(WS-TX)
                   MOVE WS-FEE-SCHEDX-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3740-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHEDX TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHEDX-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-EXCEPT-AMT NOT < WS-FEE-SCHEDX-LOW(WS-TX)
                   MOVE WS-FEE-SCHEDX-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E894 REF NO REQUIRED
      * E050 DEPOSIT YTD NEGATIVE
      * E886 DEPOSIT YTD OVER 25000
      * E008 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD < 0
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD > 25000.00
                   MOVE 'E886' TO WS-REJECT-CODE
               WHEN LBXM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E008' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN POST DATE IS PAST
      * FMZ -> KSL WHEN POST DATE IS PAST
      * KSL -> NYK WHEN POST DATE IS PAST
      * NYK -> DVP WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 10
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-TRAN-CD
                   ADD LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3810-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED = LBXM-FEE-AMT
                       * LBXM-PENALTY-PCT / 200
               WHEN LBXM-REGION-CD = 'DVP'
                   MOVE 10.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN LBXM-REGION-CD = 'FMZ'
                   MOVE 3.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED = LBXM-FEE-AMT
                       * LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-AMT > 0 AND LBXM-CASH-ON-HAND-AMT < 5.00
               MOVE 5.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 25.00
               MOVE 25.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E163 NAME REQUIRED
      * E887 CHECK AMT NEGATIVE
      * E244 CHECK AMT OVER 10000
      * E144 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-NAME = SPACES
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT < 0
                   MOVE 'E887' TO WS-REJECT-CODE
               WHEN LBXT-CHECK-AMT > 10000.00
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN LBXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-FEE.
           IF LBXM-FEE-AMT < 25.00
               MOVE 25.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-AMT > 50000.00
               MOVE 50000.00 TO LBXM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3850-FLAG-FLOAT.
           IF LBXM-FLOAT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-MATURITY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-DEPOSIT.
           IF LBXM-DEPOSIT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN LAST STMT DATE IS PAST
      * D -> F WHEN LAST STMT DATE IS PAST
      * F -> K WHEN LAST STMT DATE IS PAST
      * K -> U WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN EFF DATE IS PAST
      * UK -> VR WHEN EFF DATE IS PAST
      * VR -> XG WHEN EFF DATE IS PAST
      * XG -> GF WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHECK YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXM-CHECK-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-CHECK-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-LAST-STMT.
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
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-DEPOSIT.
           IF LBXM-DEPOSIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-DEPOSIT-YTD * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FLOAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE LBXT-FLOAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3960-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 5
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-CYCLE.
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
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3980-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBXM-DEPOSIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-DEPOSIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXM-CASH-ON-HAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN NOTICE DATE IS PAST
      * D -> F WHEN NOTICE DATE IS PAST
      * F -> K WHEN NOTICE DATE IS PAST
      * K -> U WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 3
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4040-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FLOAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-HOLD-FLAG = 'Y'
                   COMPUTE LBXM-FLOAT-YTD ROUNDED = WS-WORK-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-REGION-CD = 'DVP'
                   MOVE 12.00 TO LBXM-FLOAT-YTD
               WHEN LBXM-REGION-CD = 'FMZ'
                   MOVE 5.00 TO LBXM-FLOAT-YTD
               WHEN OTHER
                   COMPUTE LBXM-FLOAT-YTD ROUNDED = WS-WORK-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-FLOAT-YTD > 0 AND LBXM-FLOAT-YTD < 5.00
               MOVE 5.00 TO LBXM-FLOAT-YTD
           END-IF
           IF LBXM-FLOAT-YTD > 75.00
               MOVE 75.00 TO LBXM-FLOAT-YTD
           END-IF
           ADD LBXM-FLOAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN MATURITY DATE IS PAST
      * JM -> WW WHEN MATURITY DATE IS PAST
      * WW -> XE WHEN MATURITY DATE IS PAST
      * XE -> DX WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD LBXM-CHECK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RC -> DP WHEN MATURITY DATE IS PAST
      * DP -> EX WHEN MATURITY DATE IS PAST
      * EX -> RJ WHEN MATURITY DATE IS PAST
      * RJ -> AD WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4090-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4100-AGE-NOTICE.
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
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN LAST STMT DATE IS PAST
      * FMZ -> KSL WHEN LAST STMT DATE IS PAST
      * KSL -> NYK WHEN LAST STMT DATE IS PAST
      * NYK -> DVP WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       4120-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-YTD
           END-IF
           ADD LBXM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4130-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT FEE ON THE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4140-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FLOAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE LBXM-FLOAT-AMT ROUNDED = LBXM-FEE-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 20.00 TO LBXM-FLOAT-AMT
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 3.00 TO LBXM-FLOAT-AMT
               WHEN OTHER
                   COMPUTE LBXM-FLOAT-AMT ROUNDED = LBXM-FEE-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-FLOAT-AMT > 0 AND LBXM-FLOAT-AMT < 10.00
               MOVE 10.00 TO LBXM-FLOAT-AMT
           END-IF
           IF LBXM-FLOAT-AMT > 25.00
               MOVE 25.00 TO LBXM-FLOAT-AMT
           END-IF
           ADD LBXM-FLOAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE CHECK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXT-CHECK-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4160-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E829 CONTACT REQUIRED
      * E566 FEE AMT NEGATIVE
      * E797 FEE AMT OVER 25000
      * E927 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT < 0
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT > 25000.00
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN LBXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 12
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4200-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   COMPUTE LBXT-FEE-AMT ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 20.00 TO LBXT-FEE-AMT
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 3.00 TO LBXT-FEE-AMT
               WHEN OTHER
                   COMPUTE LBXT-FEE-AMT ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXT-FEE-AMT > 0 AND LBXT-FEE-AMT < 5.00
               MOVE 5.00 TO LBXT-FEE-AMT
           END-IF
           IF LBXT-FEE-AMT > 25.00
               MOVE 25.00 TO LBXT-FEE-AMT
           END-IF
           ADD LBXT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E769 REF NO REQUIRED
      * E849 DEPOSIT AMT NEGATIVE
      * E931 DEPOSIT AMT OVER 250000
      * E336 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-AMT < 0
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-AMT > 250000.00
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN LBXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E336' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK AMT BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-LOCKBOX-CD
                   ADD LBXT-CHECK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-CHECK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4230-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
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
                   MOVE LBFEE-EXCEPT-AMT
                     TO LBXM-EXCEPT-AMT
                   MOVE LBFEE-FLOAT-AMT
                     TO LBXM-FLOAT-AMT
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
                   MOVE LBCAL-CHECK-AMT
                     TO LBXM-CHECK-AMT
                   MOVE LBCAL-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBCAL-REMIT-YTD
                     TO LBXM-REMIT-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-LBCAL' TO ABND-PARAGRAPH
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
                   MOVE LBWAIV-EXCEPT-AMT
                     TO LBXM-EXCEPT-AMT
                   MOVE LBWAIV-STOP-FLAG
                     TO LBXM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-LBWAIV' TO ABND-PARAGRAPH
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
           DISPLAY 'LBXEXCP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'LBXEXCP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
