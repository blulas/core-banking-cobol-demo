      *================================================================*
      * PROGRAM  : TXXNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELEX WIRE MESSAGING - CUSTOMER NOTICES. READS THE  *
      *            TLX TRANSACTIONS, UPDATES THE TLX MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. TESTED TELEX FUNDS     *
      *            TRANSFER MESSAGES AND TEST KEY VALIDATION           *
      * INPUT    : TLXMST (KSDS), TLXTRN, BANKCTL                      *
      * OUTPUT   : TLXOUT, TXXNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TXCAL, TXPRC            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-03-08 EWS  ORIGINAL                                       *
      * 1995-06-18 LMB  COVER EXCEPTIONS TO OPS QUEUE REQ7269          *
      * 1996-08-13 RJT  PERFORMANCE - CORRESP READ AHEAD INC9839       *
      * 2003-12-14 CRP  CHARGE STATUS CODES ADDED INC6848              *
      * 2006-06-12 LMB  COVER CALCULATION CORRECTED CHG7770            *
      * 2006-05-25 GAF  PERFORMANCE - CHARGE READ AHEAD PRB3200        *
      * 2006-04-15 RJT  AUDIT FINDING - QUEUED CONTROL TOTALS CHG7078  *
      * 2007-07-24 LMB  COVER RERUN PROTECTION REQ4484                 *
      * 2008-08-18 TWH  CORRESP TABLE EXPANDED REQ6372                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXXNOTC.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TLXMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TLXM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TLXTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TLXOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TXXNORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TLXMST.
       FD  TRAN-FILE.
       COPY TLXTRN.
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
      * WS-CORRESP-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CORRESP-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000240500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000889000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000678000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000200000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025500000000564000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025750000000111000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025800000000148000'.
       01  WS-CORRESP-BAND-TBL REDEFINES WS-CORRESP-BAND-VALUES.
           05  WS-CORRESP-BAND-ROW OCCURS 7 TIMES.
               10  WS-CORRESP-BAND-LOW     PIC 9(09)V99.
               10  WS-CORRESP-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CHARGE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CHARGE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000771500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000203000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000229500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000541000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000289500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000296000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016600000000862500'.
       01  WS-CHARGE-TIER-TBL REDEFINES WS-CHARGE-TIER-VALUES.
           05  WS-CHARGE-TIER-ROW OCCURS 7 TIMES.
               10  WS-CHARGE-TIER-LOW      PIC 9(09)V99.
               10  WS-CHARGE-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COVER-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COVER-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000399000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000429000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000288500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000266000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000371000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000408500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000055000'.
       01  WS-COVER-BAND-TBL REDEFINES WS-COVER-BAND-VALUES.
           05  WS-COVER-BAND-ROW OCCURS 7 TIMES.
               10  WS-COVER-BAND-LOW       PIC 9(09)V99.
               10  WS-COVER-BAND-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TXXNOTC  TELEX WIRE MESSAGING'.
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
       COPY TXCALP.
       COPY TXPRCP.

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
               MOVE 'TLXMST' TO ABND-FILE-NAME
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
           MOVE TLXT-KEY TO TLXM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-REASON
               PERFORM 3010-DESCRIBE-TRAN
               PERFORM 3020-CHECK-LAST-PAY
               PERFORM 3030-SELECT-FEE-PCT
               PERFORM 3040-EDIT-ADDR
               PERFORM 3050-CHECK-NOTICE
               PERFORM 3060-ACCUM-REGION
               PERFORM 3070-SET-MSG-TYPE
               PERFORM 3080-AGE-EFFECTIVE
               PERFORM 3090-COUNT-ITEM
               PERFORM 3100-COUNT-SEQ
               PERFORM 3110-SPLIT-CORRESP
               PERFORM 3120-DESCRIBE-REASON
               PERFORM 3130-AGE-CLOSE
               PERFORM 3140-ASSESS-OVERRIDE
               PERFORM 3150-ASSESS-PREFERRED
               PERFORM 3160-ACCRUE-SETTLE
               PERFORM 3170-SPLIT-CORRESP
               PERFORM 3180-FLAG-QUEUED
               PERFORM 3190-PRORATE-QUEUED
               PERFORM 3200-EDIT-ADDR
               PERFORM 3210-SELECT-RATE
               PERFORM 3220-PRORATE-SETTLE
               PERFORM 3230-SET-TYPE
               PERFORM 3240-SELECT-RATE
               PERFORM 3250-SET-TEST-KEY
               PERFORM 3260-FLAG-SETTLE
               PERFORM 3270-DESCRIBE-BRANCH
               PERFORM 3280-SPLIT-SETTLE
               PERFORM 3290-ACCRUE-WIRE
               PERFORM 3300-CHECK-NEXT-REVIEW
               PERFORM 3310-COUNT-EXCEPT
               PERFORM 3320-PRORATE-COVER
               PERFORM 3330-ASSESS-STOP
               PERFORM 3340-LIMIT-WIRE
               PERFORM 3350-LIMIT-CHARGE
               PERFORM 3360-FLAG-COVER
               PERFORM 3370-EDIT-OPERATOR
               PERFORM 3380-AGE-NEXT-DUE
               PERFORM 3390-SPLIT-WIRE
               PERFORM 3400-ACCUM-REASON
               PERFORM 3410-EDIT-CUST-ID
               PERFORM 3420-COUNT-NOTICE
               PERFORM 3430-ACCUM-REGION
               PERFORM 3440-ACCRUE-COVER
               PERFORM 3450-SPLIT-CORRESP
               PERFORM 3460-PRORATE-QUEUED
               PERFORM 3470-CHECK-NOTICE
               PERFORM 3480-SET-CHANNEL
               PERFORM 3490-ACCRUE-CHARGE
               PERFORM 3500-AGE-NEXT-DUE
               PERFORM 3510-ACCRUE-SETTLE
               PERFORM 3520-CHECK-LAST-PAY
               PERFORM 3530-EDIT-ADDR
               PERFORM 3540-LIMIT-COVER
               PERFORM 3550-ACCUM-BRANCH
               PERFORM 3560-SELECT-RATE
               PERFORM 3570-PRORATE-SETTLE
               PERFORM 3580-SELECT-DISCOUNT
               PERFORM 3590-EDIT-CONTACT
               PERFORM 3600-DESCRIBE-TEST-KEY
               PERFORM 3610-EDIT-NAME
               PERFORM 3620-ACCRUE-CHARGE
               PERFORM 3630-DESCRIBE-TRAN
               PERFORM 3640-COUNT-NOTICE
               PERFORM 3650-PRORATE-COVER
               PERFORM 3660-FLAG-WIRE
               PERFORM 3670-DESCRIBE-TYPE
               PERFORM 3680-ACCUM-REGION
               PERFORM 3690-ACCRUE-QUEUED
               PERFORM 3700-ASSESS-REVERSAL
               PERFORM 3710-LIMIT-CHARGE
               PERFORM 3720-SET-TYPE
               PERFORM 3730-DESCRIBE-TEST-KEY
               PERFORM 3740-ACCUM-REASON
               PERFORM 3750-LIMIT-SETTLE
               PERFORM 3760-SPLIT-WIRE
               PERFORM 3770-COUNT-ITEM
               PERFORM 3780-SPLIT-COVER
               PERFORM 3790-EDIT-CONTACT
               PERFORM 3800-LIMIT-WIRE
               PERFORM 3810-SELECT-FEE-PCT
               PERFORM 3820-LIMIT-SETTLE
               PERFORM 3830-SET-TYPE
               PERFORM 3840-SET-CHANNEL
               PERFORM 3850-EDIT-OPERATOR
               PERFORM 3860-LIMIT-CORRESP
               PERFORM 3870-AGE-EFFECTIVE
               PERFORM 3880-PRORATE-WIRE
               PERFORM 3890-EDIT-REJECT
               PERFORM 3900-FLAG-WIRE
               PERFORM 3910-AGE-NOTICE
               PERFORM 3920-LIMIT-WIRE
               PERFORM 3930-EDIT-DESC
               PERFORM 3940-PRORATE-QUEUED
               PERFORM 3950-CHECK-NEXT-DUE
               PERFORM 3960-SPLIT-WIRE
               PERFORM 3970-SET-TEST-KEY
               PERFORM 3980-AGE-LAST-PAY
               PERFORM 3990-SPLIT-COVER
               PERFORM 4000-ACCUM-TYPE
               PERFORM 4010-CHECK-EFF
               PERFORM 4020-COUNT-ITEM
               PERFORM 4030-ASSESS-PREFERRED
               PERFORM 4040-CHECK-EXPIRY
               PERFORM 4050-LIMIT-SETTLE
               PERFORM 4060-SPLIT-CORRESP
               PERFORM 4070-CHECK-OPEN
               PERFORM 4080-FLAG-ITEM
               PERFORM 4090-LIMIT-CHARGE
               PERFORM 4100-ACCUM-CHANNEL
               PERFORM 4110-CHECK-NOTICE
               PERFORM 4120-ASSESS-SUSPEND
               PERFORM 4130-LIMIT-SETTLE
               PERFORM 4140-FLAG-COVER
               PERFORM 4150-ACCRUE-COVER
               PERFORM 4160-ASSESS-NOTICE-SENT
               PERFORM 4170-LIMIT-QUEUED
               PERFORM 4180-DESCRIBE-TYPE
               PERFORM 4190-SELECT-RATE
               PERFORM 4200-FLAG-WIRE
               PERFORM 4210-SPLIT-SETTLE
               PERFORM 4220-COUNT-NOTICE
               PERFORM 4230-AGE-POST
               PERFORM 4240-EDIT-CONTACT
               PERFORM 4250-ACCUM-MSG-TYPE
               PERFORM 4260-ASSESS-OVERRIDE
               PERFORM 4270-LIMIT-CHARGE
               PERFORM 4280-PRORATE-COVER
               PERFORM 4290-SPLIT-CHARGE
               PERFORM 4300-COUNT-ITEM
               PERFORM 4310-CHECK-LAST-PAY
               PERFORM 4320-LIMIT-CHARGE
               PERFORM 4330-SPLIT-QUEUED
               PERFORM 4340-SELECT-DISCOUNT
               PERFORM 4350-SPLIT-WIRE
               PERFORM 4360-SELECT-RATE
               PERFORM 4370-SET-MSG-TYPE
               PERFORM 4380-SET-TRAN
               PERFORM 4390-SELECT-RATE
               PERFORM 4400-COUNT-PAYMENT
               PERFORM 4410-ACCRUE-WIRE
               PERFORM 4420-AGE-EXPIRY
               PERFORM 4430-SET-CHANNEL
               PERFORM 4440-LIMIT-WIRE
               PERFORM 4450-PRORATE-CHARGE
               PERFORM 4460-SPLIT-CHARGE
               PERFORM 4470-SPLIT-COVER
               PERFORM 4480-LIMIT-CORRESP
               PERFORM 4490-CHECK-OPEN
               PERFORM 4500-ACCUM-CHANNEL
               PERFORM 4510-SET-BRANCH
               PERFORM 4520-ACCUM-REGION
               PERFORM 4530-AGE-CLOSE
               PERFORM 4540-SPLIT-COVER
               PERFORM 4550-EDIT-DESC
               PERFORM 4560-ACCRUE-WIRE
               PERFORM 4570-SPLIT-SETTLE
               PERFORM 4580-SELECT-DISCOUNT
               PERFORM 4590-CHECK-LAST-PAY
               PERFORM 4600-LIMIT-COVER
               PERFORM 4610-ASSESS-NOTICE-SENT
               PERFORM 4620-PRORATE-COVER
               PERFORM 4630-AGE-NOTICE
               PERFORM 4640-LIMIT-CORRESP
               PERFORM 4650-DESCRIBE-BRANCH
               PERFORM 4660-SPLIT-QUEUED
               PERFORM 4670-AGE-OPEN
               PERFORM 7030-CALL-TXCAL
               PERFORM 7020-CALL-TXPRC
               REWRITE TLX-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TLXMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TLX-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN EFF DATE IS PAST
      * JBT -> LSS WHEN EFF DATE IS PAST
      * LSS -> RDP WHEN EFF DATE IS PAST
      * RDP -> RPL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'OT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHARGE TIER TIER FOR THE CORRESP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGE-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-CORRESP-AMT NOT < WS-CHARGE-TIER-LOW(WS-TX)
                   MOVE WS-CHARGE-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E682 ADDR REQUIRED
      * E304 CORRESP AMT NEGATIVE
      * E202 CORRESP AMT OVER 5000
      * E750 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-ADDR = SPACES
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN TLXM-CORRESP-AMT < 0
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN TLXM-CORRESP-AMT > 5000.00
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN TLXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3060-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXT-WIRE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-WIRE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN NEXT REVIEW DATE IS PAST
      * LX -> ND WHEN NEXT REVIEW DATE IS PAST
      * ND -> PT WHEN NEXT REVIEW DATE IS PAST
      * PT -> LG WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 12
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-YTD - (WS-WORK-AMT * 12)
           COMPUTE TLXM-WIRE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3140-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLXM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO TLXM-SETTLE-AMT
               WHEN TLXT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO TLXM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-SETTLE-AMT > 0 AND TLXM-SETTLE-AMT < 10.00
               MOVE 10.00 TO TLXM-SETTLE-AMT
           END-IF
           IF TLXM-SETTLE-AMT > 100.00
               MOVE 100.00 TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE COVER FEE ON THE CHARGE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3150-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TLXM-COVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE TLXM-COVER-AMT ROUNDED = TLXM-CHARGE-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-TYPE-CD = 'K'
                   MOVE 12.00 TO TLXM-COVER-AMT
               WHEN TLXM-TYPE-CD = 'L'
                   MOVE 12.00 TO TLXM-COVER-AMT
               WHEN OTHER
                   COMPUTE TLXM-COVER-AMT ROUNDED = TLXM-CHARGE-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-COVER-AMT > 0 AND TLXM-COVER-AMT < 10.00
               MOVE 10.00 TO TLXM-COVER-AMT
           END-IF
           IF TLXM-COVER-AMT > 50.00
               MOVE 50.00 TO TLXM-COVER-AMT
           END-IF
           ADD TLXM-COVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-SETTLE.
           IF TLXM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-YTD * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-YTD - (WS-WORK-AMT * 3)
           COMPUTE TLXT-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3180-FLAG-QUEUED.
           IF TLXM-QUEUED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-QUEUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-AMT ROUNDED =
                   TLXM-CORRESP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E430 ADDR REQUIRED
      * E953 QUEUED AMT NEGATIVE
      * E797 QUEUED AMT OVER 10000
      * E011 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-ADDR = SPACES
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN TLXT-QUEUED-AMT < 0
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN TLXT-QUEUED-AMT > 10000.00
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE WIRE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-WIRE-YTD NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-AMT ROUNDED =
                   TLXM-CHARGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> P WHEN NEXT REVIEW DATE IS PAST
      * P -> W WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE COVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-COVER-YTD NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TEST KEY CD:
      * EA -> GA WHEN NEXT DUE DATE IS PAST
      * GA -> HL WHEN NEXT DUE DATE IS PAST
      * HL -> JC WHEN NEXT DUE DATE IS PAST
      * JC -> KC WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GA' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GA'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3260-FLAG-SETTLE.
           IF TLXT-SETTLE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXT-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-WIRE.
           IF TLXM-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXM-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       3320-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-COVER-AMT ROUNDED =
                   TLXM-QUEUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3330-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLXT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXT-SETTLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-TYPE-CD = 'K'
                   MOVE 10.00 TO TLXT-SETTLE-AMT
               WHEN TLXM-TYPE-CD = 'L'
                   MOVE 7.00 TO TLXT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXT-SETTLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXT-SETTLE-AMT > 0 AND TLXT-SETTLE-AMT < 10.00
               MOVE 10.00 TO TLXT-SETTLE-AMT
           END-IF
           IF TLXT-SETTLE-AMT > 50.00
               MOVE 50.00 TO TLXT-SETTLE-AMT
           END-IF
           ADD TLXT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-WIRE.
           IF TLXT-WIRE-AMT < 10.00
               MOVE 10.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-WIRE-AMT > 10000.00
               MOVE 10000.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3350-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 10.00
               MOVE 10.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 50000.00
               MOVE 50000.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COVER AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3360-FLAG-COVER.
           IF TLXM-COVER-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-COVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E755 OPERATOR REQUIRED
      * E556 CHARGE YTD NEGATIVE
      * E058 CHARGE YTD OVER 50000
      * E106 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3370-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD < 0
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD > 50000.00
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN TLXT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WIRE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXM-WIRE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-WIRE-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXM-COVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REASON-CD
                   ADD TLXM-QUEUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-QUEUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E217 CUST ID REQUIRED
      * E345 COVER AMT NEGATIVE
      * E292 COVER AMT OVER 25000
      * E100 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CUST-ID = SPACES
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN TLXT-COVER-AMT < 0
                   MOVE 'E345' TO WS-REJECT-CODE
               WHEN TLXT-COVER-AMT > 25000.00
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN TLXT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-WIRE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-WIRE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-COVER.
           IF TLXM-COVER-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-COVER-YTD * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXT-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXM-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       3460-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-AMT ROUNDED =
                   TLXM-CHARGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-CHARGE.
           IF TLXM-CHARGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-YTD * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-QUEUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-SETTLE.
           IF TLXM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-YTD * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E156 ADDR REQUIRED
      * E851 WIRE YTD NEGATIVE
      * E772 WIRE YTD OVER 10000
      * E012 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-ADDR = SPACES
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-YTD < 0
                   MOVE 'E851' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-YTD > 10000.00
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E012' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COVER YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3540-LIMIT-COVER.
           IF TLXM-COVER-YTD < 0.00
               MOVE 0.00 TO TLXM-COVER-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-COVER-YTD > 500.00
               MOVE 500.00 TO TLXM-COVER-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-BRANCH-CD
                   ADD TLXM-CORRESP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE CHARGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-CHARGE-YTD NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-AMT ROUNDED =
                   TLXM-COVER-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP BAND TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-BAND-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-WIRE-AMT NOT < WS-CORRESP-BAND-LOW(WS-TX)
                   MOVE WS-CORRESP-BAND-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E148 CONTACT REQUIRED
      * E303 WORK AMT NEGATIVE
      * E401 WORK AMT OVER 50000
      * E773 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E303' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TEST KEY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E289 NAME REQUIRED
      * E917 WIRE AMT NEGATIVE
      * E544 WIRE AMT OVER 5000
      * E339 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-NAME = SPACES
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT < 0
                   MOVE 'E917' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT > 5000.00
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E339' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-CHARGE.
           IF TLXM-CHARGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-YTD * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'OT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3650-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TLXM-COVER-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WIRE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3660-FLAG-WIRE.
           IF TLXT-WIRE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXT-WIRE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-CHARGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CHARGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-QUEUED.
           IF TLXM-QUEUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-QUEUED-AMT * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WIRE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3700-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLXM-WIRE-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-MSG-TYPE-CD = 'LG'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN TLXM-MSG-TYPE-CD = 'LX'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLXM-WIRE-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 10.00
               MOVE 10.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 10000.00
               MOVE 10000.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN EXPIRY DATE IS PAST
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> P WHEN EXPIRY DATE IS PAST
      * P -> W WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TEST KEY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REASON-CD
                   ADD TLXM-CHARGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CHARGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-SETTLE.
           IF TLXT-SETTLE-AMT < 25.00
               MOVE 25.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-SETTLE-AMT > 5000.00
               MOVE 5000.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WIRE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXM-WIRE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-WIRE-YTD - (WS-WORK-AMT * 2)
           COMPUTE TLXM-QUEUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3770-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 5
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXT-COVER-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXT-COVER-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-COVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E666 CONTACT REQUIRED
      * E140 SETTLE YTD NEGATIVE
      * E181 SETTLE YTD OVER 250000
      * E094 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD < 0
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD > 250000.00
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN TLXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3800-LIMIT-WIRE.
           IF TLXM-WIRE-AMT < 25.00
               MOVE 25.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-AMT > 500.00
               MOVE 500.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHARGE TIER TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGE-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXT-WIRE-AMT NOT < WS-CHARGE-TIER-LOW(WS-TX)
                   MOVE WS-CHARGE-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3820-LIMIT-SETTLE.
           IF TLXM-SETTLE-YTD < 0.00
               MOVE 0.00 TO TLXM-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-YTD > 50000.00
               MOVE 50000.00 TO TLXM-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN LAST PAY DATE IS PAST
      * L -> M WHEN LAST PAY DATE IS PAST
      * M -> P WHEN LAST PAY DATE IS PAST
      * P -> W WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E535 OPERATOR REQUIRED
      * E725 CHARGE YTD NEGATIVE
      * E896 CHARGE YTD OVER 250000
      * E650 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD < 0
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD > 250000.00
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN TLXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-CORRESP.
           IF TLXM-CORRESP-YTD < 100.00
               MOVE 100.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-YTD > 5000.00
               MOVE 5000.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WIRE AMT.
      *----------------------------------------------------------------*
       3880-PRORATE-WIRE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-WIRE-AMT ROUNDED =
                   TLXT-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-WIRE-AMT
           END-IF
           ADD TLXM-WIRE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E798 REJECT CD REQUIRED
      * E377 COVER YTD NEGATIVE
      * E437 COVER YTD OVER 99999
      * E124 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-REJECT-CD = SPACES
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD < 0
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD > 99999.00
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN TLXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3900-FLAG-WIRE.
           IF TLXM-WIRE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-WIRE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-WIRE.
           IF TLXM-WIRE-AMT < 100.00
               MOVE 100.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-AMT > 10000.00
               MOVE 10000.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E695 DESC REQUIRED
      * E445 WIRE AMT NEGATIVE
      * E729 WIRE AMT OVER 10000
      * E398 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT < 0
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT > 10000.00
                   MOVE 'E729' TO WS-REJECT-CODE
               WHEN TLXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED YTD.
      *----------------------------------------------------------------*
       3940-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-YTD ROUNDED =
                   TLXM-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-QUEUED-YTD
           END-IF
           ADD TLXM-QUEUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WIRE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXM-WIRE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-WIRE-YTD - (WS-WORK-AMT * 2)
           COMPUTE TLXT-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TEST KEY CD:
      * EA -> GA WHEN EXPIRY DATE IS PAST
      * GA -> HL WHEN EXPIRY DATE IS PAST
      * HL -> JC WHEN EXPIRY DATE IS PAST
      * JC -> KC WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GA' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GA'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXT-COVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXT-COVER-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXT-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 6
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP FEE ON THE COVER AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4030-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CORRESP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-COVER-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO TLXM-CORRESP-AMT
               WHEN TLXT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO TLXM-CORRESP-AMT
               WHEN OTHER
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-COVER-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-CORRESP-AMT > 0 AND TLXM-CORRESP-AMT < 2.00
               MOVE 2.00 TO TLXM-CORRESP-AMT
           END-IF
           IF TLXM-CORRESP-AMT > 35.00
               MOVE 35.00 TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-SETTLE.
           IF TLXT-SETTLE-AMT < 10.00
               MOVE 10.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-SETTLE-AMT > 1000.00
               MOVE 1000.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-YTD - (WS-WORK-AMT * 2)
           COMPUTE TLXT-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4080-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4090-LIMIT-CHARGE.
           IF TLXM-CHARGE-YTD < 0.00
               MOVE 0.00 TO TLXM-CHARGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-YTD > 500.00
               MOVE 500.00 TO TLXM-CHARGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD TLXT-COVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-COVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE COVER YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4120-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLXM-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXM-QUEUED-AMT ROUNDED = TLXM-COVER-YTD *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 20.00 TO TLXM-QUEUED-AMT
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 20.00 TO TLXM-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXM-QUEUED-AMT ROUNDED = TLXM-COVER-YTD *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-QUEUED-AMT > 0 AND TLXM-QUEUED-AMT < 5.00
               MOVE 5.00 TO TLXM-QUEUED-AMT
           END-IF
           IF TLXM-QUEUED-AMT > 35.00
               MOVE 35.00 TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 50.00
               MOVE 50.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 1000.00
               MOVE 1000.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COVER AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4140-FLAG-COVER.
           IF TLXM-COVER-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-COVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-COVER.
           IF TLXM-COVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-COVER-AMT * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGE YTD FEE ON THE QUEUED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4160-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CHARGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE TLXM-CHARGE-YTD ROUNDED = TLXT-QUEUED-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO TLXM-CHARGE-YTD
               WHEN TLXT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO TLXM-CHARGE-YTD
               WHEN OTHER
                   COMPUTE TLXM-CHARGE-YTD ROUNDED = TLXT-QUEUED-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-CHARGE-YTD > 0 AND TLXM-CHARGE-YTD < 5.00
               MOVE 5.00 TO TLXM-CHARGE-YTD
           END-IF
           IF TLXM-CHARGE-YTD > 75.00
               MOVE 75.00 TO TLXM-CHARGE-YTD
           END-IF
           ADD TLXM-CHARGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4170-LIMIT-QUEUED.
           IF TLXM-QUEUED-YTD < 100.00
               MOVE 100.00 TO TLXM-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-QUEUED-YTD > 5000.00
               MOVE 5000.00 TO TLXM-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE WIRE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-WIRE-YTD NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4200-FLAG-WIRE.
           IF TLXM-WIRE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-WIRE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E736 CONTACT REQUIRED
      * E008 WORK AMT 2 NEGATIVE
      * E328 WORK AMT 2 OVER 10000
      * E860 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E008' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN TLXM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E860' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGE YTD BY MSG TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-MSG-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-MSG-TYPE-CD
                   ADD TLXM-CHARGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-MSG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CHARGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGE FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4260-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CHARGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXM-CHARGE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 3.00 TO TLXM-CHARGE-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 7.00 TO TLXM-CHARGE-AMT
               WHEN OTHER
                   COMPUTE TLXM-CHARGE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-CHARGE-AMT > 0 AND TLXM-CHARGE-AMT < 1.00
               MOVE 1.00 TO TLXM-CHARGE-AMT
           END-IF
           IF TLXM-CHARGE-AMT > 150.00
               MOVE 150.00 TO TLXM-CHARGE-AMT
           END-IF
           ADD TLXM-CHARGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 50.00
               MOVE 50.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 50000.00
               MOVE 50000.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       4280-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-COVER-AMT ROUNDED =
                   TLXM-QUEUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CHARGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4320-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 100.00
               MOVE 100.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 50000.00
               MOVE 50000.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4330-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXT-QUEUED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXT-QUEUED-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE COVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-COVER-AMT NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WIRE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4350-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXM-WIRE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-WIRE-YTD - (WS-WORK-AMT * 2)
           COMPUTE TLXM-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4360-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN CLOSE DATE IS PAST
      * LX -> ND WHEN CLOSE DATE IS PAST
      * ND -> PT WHEN CLOSE DATE IS PAST
      * PT -> LG WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4370-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> OT WHEN NEXT REVIEW DATE IS PAST
      * OT -> RJ WHEN NEXT REVIEW DATE IS PAST
      * RJ -> RP WHEN NEXT REVIEW DATE IS PAST
      * RP -> CN WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4380-SET-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'OT' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'OT'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RJ' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RP' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RP'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CN' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-QUEUED-AMT NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4400-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4410-ACCRUE-WIRE.
           IF TLXT-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-WIRE-AMT * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4420-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4430-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4440-LIMIT-WIRE.
           IF TLXT-WIRE-AMT < 10.00
               MOVE 10.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-WIRE-AMT > 10000.00
               MOVE 10000.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHARGE AMT.
      *----------------------------------------------------------------*
       4450-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CHARGE-AMT ROUNDED =
                   TLXM-CORRESP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-CHARGE-AMT
           END-IF
           ADD TLXM-CHARGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CHARGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXM-COVER-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-COVER-YTD - (WS-WORK-AMT * 12)
           COMPUTE TLXT-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4480-LIMIT-CORRESP.
           IF TLXM-CORRESP-AMT < 100.00
               MOVE 100.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-AMT > 10000.00
               MOVE 10000.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4490-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4500-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD TLXM-COVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-COVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN EXPIRY DATE IS PAST
      * KR -> LD WHEN EXPIRY DATE IS PAST
      * LD -> LS WHEN EXPIRY DATE IS PAST
      * LS -> XL WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4510-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4520-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4530-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4540-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXM-COVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-COVER-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXT-COVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E401 DESC REQUIRED
      * E610 WIRE YTD NEGATIVE
      * E615 WIRE YTD OVER 25000
      * E064 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4550-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-YTD < 0
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-YTD > 25000.00
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4560-ACCRUE-WIRE.
           IF TLXM-WIRE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-YTD * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4570-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHARGE TIER TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGE-TIER-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXM-WIRE-AMT NOT < WS-CHARGE-TIER-LOW(WS-TX)
                   MOVE WS-CHARGE-TIER-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4590-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TLXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COVER AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4600-LIMIT-COVER.
           IF TLXT-COVER-AMT < 0.00
               MOVE 0.00 TO TLXT-COVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-COVER-AMT > 1000.00
               MOVE 1000.00 TO TLXT-COVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE QUEUED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4610-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = TLXT-QUEUED-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 5.00 TO TLXM-SETTLE-AMT
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 15.00 TO TLXM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = TLXT-QUEUED-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-SETTLE-AMT > 0 AND TLXM-SETTLE-AMT < 5.00
               MOVE 5.00 TO TLXM-SETTLE-AMT
           END-IF
           IF TLXM-SETTLE-AMT > 35.00
               MOVE 35.00 TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CORRESP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       4620-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-COVER-AMT ROUNDED =
                   TLXM-CORRESP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4630-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4640-LIMIT-CORRESP.
           IF TLXM-CORRESP-AMT < 100.00
               MOVE 100.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-AMT > 10000.00
               MOVE 10000.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4650-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4660-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE TLXM-QUEUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4670-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-TXCAL.
           INITIALIZE TXCAL-PARMS
           MOVE TLXM-CHARGE-AMT
             TO TXCAL-CHARGE-AMT
           MOVE TLXM-SETTLE-AMT
             TO TXCAL-SETTLE-AMT
           MOVE TLXM-QUEUED-YTD
             TO TXCAL-QUEUED-YTD
           MOVE TLXM-CHARGE-YTD
             TO TXCAL-CHARGE-YTD
           MOVE TLXM-COVER-YTD
             TO TXCAL-COVER-YTD
           MOVE TLXM-QUEUED-AMT
             TO TXCAL-QUEUED-AMT
           MOVE TLXM-LAST-ACT-DATE
             TO TXCAL-LAST-ACT-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXCAL-NEXT-DUE-DATE
           MOVE TLXM-EXPIRY-DATE
             TO TXCAL-EXPIRY-DATE
           MOVE TLXM-NEXT-REVIEW-DATE
             TO TXCAL-NEXT-REVIEW-DATE
           MOVE TLXM-REASON-CD
             TO TXCAL-REASON-CD
           MOVE TLXM-BRANCH-CD
             TO TXCAL-BRANCH-CD
           MOVE TLXM-TYPE-CD
             TO TXCAL-TYPE-CD
           MOVE TLXM-MSG-TYPE-CD
             TO TXCAL-MSG-TYPE-CD
           MOVE TLXM-PREFERRED-FLAG
             TO TXCAL-PREFERRED-FLAG
           MOVE TLXM-REVIEW-FLAG
             TO TXCAL-REVIEW-FLAG
           MOVE TLXM-SUSPEND-FLAG
             TO TXCAL-SUSPEND-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXCAL-PAYMENT-CNT
           MOVE TLXM-EXCEPT-CNT
             TO TXCAL-EXCEPT-CNT
           MOVE TLXM-FEE-PCT
             TO TXCAL-FEE-PCT
           MOVE TLXM-RATE
             TO TXCAL-RATE
           MOVE TLXM-DESC
             TO TXCAL-DESC
           MOVE TLXM-NAME
             TO TXCAL-NAME
           CALL 'TXCAL' USING TXCAL-PARMS
           EVALUATE TXCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXCAL-QUEUED-YTD
                     TO TLXM-QUEUED-YTD
                   MOVE TXCAL-COVER-YTD
                     TO TLXM-COVER-YTD
                   MOVE TXCAL-PREFERRED-FLAG
                     TO TLXM-PREFERRED-FLAG
                   MOVE TXCAL-SUSPEND-FLAG
                     TO TLXM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-TXCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TXPRC.
           INITIALIZE TXPRC-PARMS
           MOVE TLXM-COVER-YTD
             TO TXPRC-COVER-YTD
           MOVE TLXM-QUEUED-AMT
             TO TXPRC-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXPRC-CORRESP-YTD
           MOVE TLXM-SETTLE-YTD
             TO TXPRC-SETTLE-YTD
           MOVE TLXM-QUEUED-YTD
             TO TXPRC-QUEUED-YTD
           MOVE TLXM-SETTLE-AMT
             TO TXPRC-SETTLE-AMT
           MOVE TLXM-OPEN-DATE
             TO TXPRC-OPEN-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXPRC-NEXT-DUE-DATE
           MOVE TLXM-NOTICE-DATE
             TO TXPRC-NOTICE-DATE
           MOVE TLXM-EXPIRY-DATE
             TO TXPRC-EXPIRY-DATE
           MOVE TLXM-TEST-KEY-CD
             TO TXPRC-TEST-KEY-CD
           MOVE TLXM-REGION-CD
             TO TXPRC-REGION-CD
           MOVE TLXM-BRANCH-CD
             TO TXPRC-BRANCH-CD
           MOVE TLXM-REASON-CD
             TO TXPRC-REASON-CD
           MOVE TLXM-NOTICE-SENT-FLAG
             TO TXPRC-NOTICE-SENT-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXPRC-STOP-FLAG
           MOVE TLXM-REVIEW-FLAG
             TO TXPRC-REVIEW-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXPRC-PAYMENT-CNT
           MOVE TLXM-EXCEPT-CNT
             TO TXPRC-EXCEPT-CNT
           MOVE TLXM-RATE
             TO TXPRC-RATE
           MOVE TLXM-DISCOUNT
             TO TXPRC-DISCOUNT
           MOVE TLXM-ADDR
             TO TXPRC-ADDR
           MOVE TLXM-NAME
             TO TXPRC-NAME
           CALL 'TXPRC' USING TXPRC-PARMS
           EVALUATE TXPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXPRC-COVER-YTD
                     TO TLXM-COVER-YTD
                   MOVE TXPRC-QUEUED-AMT
                     TO TLXM-QUEUED-AMT
                   MOVE TXPRC-SETTLE-AMT
                     TO TLXM-SETTLE-AMT
                   MOVE TXPRC-NOTICE-SENT-FLAG
                     TO TLXM-NOTICE-SENT-FLAG
                   MOVE TXPRC-REVIEW-FLAG
                     TO TLXM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TXPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TLXT-KEY TO RD-KEY
           MOVE TLXT-TRAN-CD TO RD-CODE
           MOVE TLXT-COVER-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TLXT-KEY TO RD-KEY
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
           DISPLAY 'TXXNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TXXNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
