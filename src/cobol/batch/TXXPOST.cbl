      *================================================================*
      * PROGRAM  : TXXPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELEX WIRE MESSAGING - DAILY POSTING. READS THE TLX *
      *            TRANSACTIONS, UPDATES THE TLX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TESTED TELEX FUNDS TRANSFER     *
      *            MESSAGES AND TEST KEY VALIDATION                    *
      * INPUT    : TLXMST (KSDS), TLXTRN, BANKCTL                      *
      * OUTPUT   : TLXOUT, TXXPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TXCAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-01-16 RJT  ORIGINAL                                       *
      * 1994-09-22 MKL  COVER STATUS CODES ADDED PRB6542               *
      * 1996-07-05 EWS  NEW COVER REPORT COLUMNS CHG1906               *
      * 1997-09-06 PQR  CHARGE CUTOFF MOVED PRB6922                    *
      * 2000-02-19 PQR  COVER EXCEPTIONS TO OPS QUEUE CHG9261          *
      * 2000-01-04 LMB  CHARGE RERUN PROTECTION PRB3202                *
      * 2008-02-26 CRP  WIRE STATUS CODES ADDED INC0708                *
      * 2009-11-19 DPS  SETTLE CALCULATION CORRECTED CHG2092           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXXPOST.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "TXXPORPT"
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
                                           VALUE 20.000000.
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
      * WS-WIRE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WIRE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000644000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000406000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000455000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000859500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004750000000023000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014750000000617000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024750000000187000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000019000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027750000000486000'.
       01  WS-WIRE-TIER-TBL REDEFINES WS-WIRE-TIER-VALUES.
           05  WS-WIRE-TIER-ROW OCCURS 9 TIMES.
               10  WS-WIRE-TIER-LOW        PIC 9(09)V99.
               10  WS-WIRE-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COVER-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COVER-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000502000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000469500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000882000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000706000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000666000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000281000'.
       01  WS-COVER-BAND-TBL REDEFINES WS-COVER-BAND-VALUES.
           05  WS-COVER-BAND-ROW OCCURS 6 TIMES.
               10  WS-COVER-BAND-LOW       PIC 9(09)V99.
               10  WS-COVER-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CORRESP-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CORRESP-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000013000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000517000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000022000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000342500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001400000000857000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001450000000574000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002450000000376000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000809000'.
       01  WS-CORRESP-GRID-TBL REDEFINES WS-CORRESP-GRID-VALUES.
           05  WS-CORRESP-GRID-ROW OCCURS 8 TIMES.
               10  WS-CORRESP-GRID-LOW     PIC 9(09)V99.
               10  WS-CORRESP-GRID-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TXXPOST  TELEX WIRE MESSAGING'.
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
               PERFORM 3000-ACCUM-BRANCH
               PERFORM 3010-SELECT-DISCOUNT
               PERFORM 3020-SET-BRANCH
               PERFORM 3030-COUNT-EXCEPT
               PERFORM 3040-SET-MSG-TYPE
               PERFORM 3050-SET-REASON
               PERFORM 3060-CHECK-EFF
               PERFORM 3070-SELECT-RATE
               PERFORM 3080-SET-BRANCH
               PERFORM 3090-CHECK-EFF
               PERFORM 3100-COUNT-PAYMENT
               PERFORM 3110-COUNT-NOTICE
               PERFORM 3120-SELECT-DISCOUNT
               PERFORM 3130-SELECT-FEE-PCT
               PERFORM 3140-DESCRIBE-BRANCH
               PERFORM 3150-CHECK-NOTICE
               PERFORM 3160-COUNT-ITEM
               PERFORM 3170-ACCUM-TYPE
               PERFORM 3180-ASSESS-REVIEW
               PERFORM 3190-COUNT-PAYMENT
               PERFORM 3200-CHECK-POST
               PERFORM 3210-COUNT-EXCEPT
               PERFORM 3220-SET-REASON
               PERFORM 3230-LIMIT-WIRE
               PERFORM 3240-EDIT-MEMO
               PERFORM 3250-LIMIT-ITEM
               PERFORM 3260-SET-MSG-TYPE
               PERFORM 3270-DESCRIBE-REGION
               PERFORM 3280-PRORATE-SETTLE
               PERFORM 3290-CHECK-EFF
               PERFORM 3300-SPLIT-WIRE
               PERFORM 3310-ACCRUE-SETTLE
               PERFORM 3320-EDIT-REJECT
               PERFORM 3330-LIMIT-SETTLE
               PERFORM 3340-SELECT-RATE
               PERFORM 3350-DESCRIBE-TEST-KEY
               PERFORM 3360-DESCRIBE-TEST-KEY
               PERFORM 3370-AGE-EFF
               PERFORM 3380-DESCRIBE-TYPE
               PERFORM 3390-EDIT-OPERATOR
               PERFORM 3400-ACCUM-CHANNEL
               PERFORM 3410-PRORATE-WIRE
               PERFORM 3420-AGE-NEXT-REVIEW
               PERFORM 3430-ACCUM-TYPE
               PERFORM 3440-SPLIT-WIRE
               PERFORM 3450-COUNT-SEQ
               PERFORM 3460-LIMIT-CHARGE
               PERFORM 3470-EDIT-NAME
               PERFORM 3480-COUNT-NOTICE
               PERFORM 3490-EDIT-CONTACT
               PERFORM 3500-CHECK-EXPIRY
               PERFORM 3510-FLAG-CHARGE
               PERFORM 3520-LIMIT-SETTLE
               PERFORM 3530-LIMIT-SETTLE
               PERFORM 3540-FLAG-SETTLE
               PERFORM 3550-ASSESS-SUSPEND
               PERFORM 3560-AGE-NOTICE
               PERFORM 3570-ASSESS-OVERRIDE
               PERFORM 3580-EDIT-REF-NO
               PERFORM 3590-LIMIT-QUEUED
               PERFORM 3600-COUNT-PAYMENT
               PERFORM 3610-SET-MSG-TYPE
               PERFORM 3620-PRORATE-SETTLE
               PERFORM 3630-EDIT-NAME
               PERFORM 3640-EDIT-CUST-ID
               PERFORM 3650-CHECK-EXPIRY
               PERFORM 3660-SPLIT-COVER
               PERFORM 3670-COUNT-SEQ
               PERFORM 3680-FLAG-WIRE
               PERFORM 3690-SPLIT-CHARGE
               PERFORM 3700-EDIT-DESC
               PERFORM 3710-FLAG-CORRESP
               PERFORM 3720-ASSESS-NOTICE-SENT
               PERFORM 3730-DESCRIBE-CHANNEL
               PERFORM 3740-LIMIT-ITEM
               PERFORM 3750-EDIT-OPERATOR
               PERFORM 3760-SPLIT-QUEUED
               PERFORM 3770-EDIT-DESC
               PERFORM 3780-LIMIT-CHARGE
               PERFORM 3790-EDIT-CUST-ID
               PERFORM 3800-LIMIT-WIRE
               PERFORM 3810-LIMIT-ITEM
               PERFORM 3820-SELECT-FEE-PCT
               PERFORM 3830-SPLIT-CORRESP
               PERFORM 3840-ASSESS-REVERSAL
               PERFORM 3850-CHECK-NEXT-DUE
               PERFORM 3860-ACCRUE-DISCOUNT
               PERFORM 3870-SET-BRANCH
               PERFORM 3880-PRORATE-COVER
               PERFORM 3890-COUNT-PAYMENT
               PERFORM 3900-ACCRUE-COVER
               PERFORM 3910-SELECT-DISCOUNT
               PERFORM 3920-PRORATE-SETTLE
               PERFORM 3930-SELECT-FEE-PCT
               PERFORM 3940-ASSESS-NOTICE-SENT
               PERFORM 3950-SPLIT-SETTLE
               PERFORM 3960-ASSESS-OVERRIDE
               PERFORM 3970-ASSESS-SUSPEND
               PERFORM 3980-SET-REASON
               PERFORM 3990-CHECK-CLOSE
               PERFORM 4000-SELECT-RATE
               PERFORM 4010-PRORATE-CORRESP
               PERFORM 4020-AGE-NOTICE
               PERFORM 4030-ACCRUE-QUEUED
               PERFORM 4040-EDIT-CONTACT
               PERFORM 4050-AGE-EXPIRY
               PERFORM 4060-AGE-EXPIRY
               PERFORM 4070-AGE-NEXT-DUE
               PERFORM 4080-FLAG-CHARGE
               PERFORM 4090-EDIT-CONTACT
               PERFORM 4100-SET-REGION
               PERFORM 4110-ASSESS-REVIEW
               PERFORM 4120-AGE-EXPIRY
               PERFORM 4130-ASSESS-PREFERRED
               PERFORM 4140-EDIT-REF-NO
               PERFORM 4150-EDIT-DESC
               PERFORM 4160-DESCRIBE-TRAN
               PERFORM 4170-SELECT-FEE-PCT
               PERFORM 4180-ACCRUE-CORRESP
               PERFORM 4190-DESCRIBE-TRAN
               PERFORM 4200-CHECK-NEXT-DUE
               PERFORM 4210-LIMIT-WIRE
               PERFORM 4220-COUNT-EXCEPT
               PERFORM 7030-CALL-TXCAL
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
      * ACCUMULATE SETTLE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-BRANCH-CD
                   ADD TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE COVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-COVER-YTD NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN EFF DATE IS PAST
      * KR -> LD WHEN EFF DATE IS PAST
      * LD -> LS WHEN EFF DATE IS PAST
      * LS -> XL WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN LAST ACT DATE IS PAST
      * LX -> ND WHEN LAST ACT DATE IS PAST
      * ND -> PT WHEN LAST ACT DATE IS PAST
      * PT -> LG WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN LAST PAY DATE IS PAST
      * JBT -> LSS WHEN LAST PAY DATE IS PAST
      * LSS -> RDP WHEN LAST PAY DATE IS PAST
      * RDP -> RPL WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-EFF.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-QUEUED-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN NOTICE DATE IS PAST
      * KR -> LD WHEN NOTICE DATE IS PAST
      * LD -> LS WHEN NOTICE DATE IS PAST
      * LS -> XL WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-EFF.
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
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE WIRE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-WIRE-YTD NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-NOTICE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 5
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD TLXT-COVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-COVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP FEE ON THE COVER YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3180-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CORRESP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-COVER-YTD *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 3.00 TO TLXM-CORRESP-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 10.00 TO TLXM-CORRESP-AMT
               WHEN OTHER
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-COVER-YTD *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-CORRESP-AMT > 0 AND TLXM-CORRESP-AMT < 1.00
               MOVE 1.00 TO TLXM-CORRESP-AMT
           END-IF
           IF TLXM-CORRESP-AMT > 25.00
               MOVE 25.00 TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TLXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN NOTICE DATE IS PAST
      * JBT -> LSS WHEN NOTICE DATE IS PAST
      * LSS -> RDP WHEN NOTICE DATE IS PAST
      * RDP -> RPL WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-WIRE.
           IF TLXT-WIRE-AMT < 25.00
               MOVE 25.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-WIRE-AMT > 50000.00
               MOVE 50000.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E905 MEMO REQUIRED
      * E013 SETTLE YTD NEGATIVE
      * E525 SETTLE YTD OVER 250000
      * E915 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E905' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD < 0
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD > 250000.00
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN EFF DATE IS PAST
      * LX -> ND WHEN EFF DATE IS PAST
      * ND -> PT WHEN EFF DATE IS PAST
      * PT -> LG WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-REGION.
           EVALUATE TLXM-REGION-CD
               WHEN 'G'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-AMT ROUNDED =
                   TLXM-CORRESP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WIRE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXM-WIRE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-WIRE-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXM-QUEUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-SETTLE.
           IF TLXM-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-AMT * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-COVER-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E525 REJECT CD REQUIRED
      * E267 QUEUED AMT NEGATIVE
      * E823 QUEUED AMT OVER 250000
      * E852 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-REJECT-CD = SPACES
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN TLXT-QUEUED-AMT < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN TLXT-QUEUED-AMT > 250000.00
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN TLXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3330-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 25.00
               MOVE 25.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 5000.00
               MOVE 5000.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXT-QUEUED-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TEST KEY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TEST KEY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
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
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E416 OPERATOR REQUIRED
      * E045 QUEUED AMT NEGATIVE
      * E113 QUEUED AMT OVER 250000
      * E668 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN TLXT-QUEUED-AMT < 0
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN TLXT-QUEUED-AMT > 250000.00
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN TLXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3410-PRORATE-WIRE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TLXM-WIRE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-REVIEW-DATE > 0 AND TLXM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD TLXM-COVER-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-COVER-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WIRE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXT-WIRE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXT-WIRE-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXT-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 5
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3460-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 100.00
               MOVE 100.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 50000.00
               MOVE 50000.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E007 NAME REQUIRED
      * E887 WORK AMT NEGATIVE
      * E924 WORK AMT OVER 10000
      * E150 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-NAME = SPACES
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E887' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN TLXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E150' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E156 CONTACT REQUIRED
      * E987 WORK AMT NEGATIVE
      * E518 WORK AMT OVER 5000
      * E471 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN TLXM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3510-FLAG-CHARGE.
           IF TLXM-CHARGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 10.00
               MOVE 10.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 10000.00
               MOVE 10000.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-SETTLE.
           IF TLXT-SETTLE-AMT < 0.00
               MOVE 0.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-SETTLE-AMT > 50000.00
               MOVE 50000.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3540-FLAG-SETTLE.
           IF TLXM-SETTLE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE YTD FEE ON THE SETTLE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3550-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLXM-WIRE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE TLXM-WIRE-YTD ROUNDED = TLXM-SETTLE-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 10.00 TO TLXM-WIRE-YTD
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 15.00 TO TLXM-WIRE-YTD
               WHEN OTHER
                   COMPUTE TLXM-WIRE-YTD ROUNDED = TLXM-SETTLE-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-WIRE-YTD > 0 AND TLXM-WIRE-YTD < 10.00
               MOVE 10.00 TO TLXM-WIRE-YTD
           END-IF
           IF TLXM-WIRE-YTD > 25.00
               MOVE 25.00 TO TLXM-WIRE-YTD
           END-IF
           ADD TLXM-WIRE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-NOTICE.
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
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE CORRESP YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3570-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLXM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = TLXM-CORRESP-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 7.00 TO TLXM-SETTLE-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 3.00 TO TLXM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = TLXM-CORRESP-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-SETTLE-AMT > 0 AND TLXM-SETTLE-AMT < 1.00
               MOVE 1.00 TO TLXM-SETTLE-AMT
           END-IF
           IF TLXM-SETTLE-AMT > 100.00
               MOVE 100.00 TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E636 REF NO REQUIRED
      * E165 WORK AMT 2 NEGATIVE
      * E393 WORK AMT 2 OVER 1000000
      * E731 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-REF-NO = SPACES
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN TLXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E731' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3590-LIMIT-QUEUED.
           IF TLXM-QUEUED-AMT < 50.00
               MOVE 50.00 TO TLXM-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-QUEUED-AMT > 500.00
               MOVE 500.00 TO TLXM-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN LAST ACT DATE IS PAST
      * LX -> ND WHEN LAST ACT DATE IS PAST
      * ND -> PT WHEN LAST ACT DATE IS PAST
      * PT -> LG WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHARGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-YTD ROUNDED =
                   TLXM-CHARGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-SETTLE-YTD
           END-IF
           ADD TLXM-SETTLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E454 NAME REQUIRED
      * E292 COVER YTD NEGATIVE
      * E100 COVER YTD OVER 1000000
      * E534 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-NAME = SPACES
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD < 0
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD > 1000000.00
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN TLXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E237 CUST ID REQUIRED
      * E368 QUEUED YTD NEGATIVE
      * E254 QUEUED YTD OVER 5000
      * E083 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CUST-ID = SPACES
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-YTD < 0
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-YTD > 5000.00
                   MOVE 'E254' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-EXPIRY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXT-COVER-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXT-COVER-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-COVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 5
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3680-FLAG-WIRE.
           IF TLXM-WIRE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-WIRE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXM-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E862 DESC REQUIRED
      * E624 WORK AMT NEGATIVE
      * E635 WORK AMT OVER 50000
      * E844 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E862' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E635' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E844' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3710-FLAG-CORRESP.
           IF TLXM-CORRESP-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CORRESP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COVER FEE ON THE CHARGE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3720-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXT-COVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXM-CHARGE-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 12.00 TO TLXT-COVER-AMT
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 10.00 TO TLXT-COVER-AMT
               WHEN OTHER
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXM-CHARGE-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-COVER-AMT > 0 AND TLXT-COVER-AMT < 2.00
               MOVE 2.00 TO TLXT-COVER-AMT
           END-IF
           IF TLXT-COVER-AMT > 35.00
               MOVE 35.00 TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3740-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E089 OPERATOR REQUIRED
      * E738 QUEUED AMT NEGATIVE
      * E532 QUEUED AMT OVER 250000
      * E931 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT < 0
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT > 250000.00
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXM-QUEUED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-QUEUED-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E073 DESC REQUIRED
      * E519 QUEUED AMT NEGATIVE
      * E109 QUEUED AMT OVER 99999
      * E105 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT < 0
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT > 99999.00
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN TLXM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3780-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 10.00
               MOVE 10.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 500.00
               MOVE 500.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E018 CUST ID REQUIRED
      * E247 SETTLE YTD NEGATIVE
      * E864 SETTLE YTD OVER 99999
      * E672 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CUST-ID = SPACES
                   MOVE 'E018' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD < 0
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD > 99999.00
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN TLXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-WIRE.
           IF TLXM-WIRE-YTD < 100.00
               MOVE 100.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-YTD > 10000.00
               MOVE 10000.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-QUEUED-YTD NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-CORRESP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE COVER AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3840-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TLXM-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXM-SETTLE-YTD ROUNDED = TLXM-COVER-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-MSG-TYPE-CD = 'LG'
                   MOVE 20.00 TO TLXM-SETTLE-YTD
               WHEN TLXM-MSG-TYPE-CD = 'LX'
                   MOVE 5.00 TO TLXM-SETTLE-YTD
               WHEN OTHER
                   COMPUTE TLXM-SETTLE-YTD ROUNDED = TLXM-COVER-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-SETTLE-YTD > 0 AND TLXM-SETTLE-YTD < 2.00
               MOVE 2.00 TO TLXM-SETTLE-YTD
           END-IF
           IF TLXM-SETTLE-YTD > 75.00
               MOVE 75.00 TO TLXM-SETTLE-YTD
           END-IF
           ADD TLXM-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN NEXT REVIEW DATE IS PAST
      * KR -> LD WHEN NEXT REVIEW DATE IS PAST
      * LD -> LS WHEN NEXT REVIEW DATE IS PAST
      * LS -> XL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER YTD.
      *----------------------------------------------------------------*
       3880-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-COVER-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-COVER-YTD
           END-IF
           ADD TLXM-COVER-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-COVER.
           IF TLXM-COVER-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-COVER-YTD * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE CORRESP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-CORRESP-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-SETTLE-AMT ROUNDED =
                   TLXM-COVER-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXT-SETTLE-AMT
           END-IF
           ADD TLXT-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COVER FEE ON THE WIRE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3940-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXT-COVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXT-WIRE-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 10.00 TO TLXT-COVER-AMT
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 12.00 TO TLXT-COVER-AMT
               WHEN OTHER
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXT-WIRE-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-COVER-AMT > 0 AND TLXT-COVER-AMT < 2.00
               MOVE 2.00 TO TLXT-COVER-AMT
           END-IF
           IF TLXT-COVER-AMT > 25.00
               MOVE 25.00 TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXT-SETTLE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXT-SETTLE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE QUEUED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3960-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP FEE ON THE QUEUED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3970-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CORRESP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-TEST-KEY-CD = 'EA'
                   MOVE 3.00 TO TLXM-CORRESP-AMT
               WHEN TLXM-TEST-KEY-CD = 'GA'
                   MOVE 15.00 TO TLXM-CORRESP-AMT
               WHEN OTHER
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-CORRESP-AMT > 0 AND TLXM-CORRESP-AMT < 5.00
               MOVE 5.00 TO TLXM-CORRESP-AMT
           END-IF
           IF TLXM-CORRESP-AMT > 75.00
               MOVE 75.00 TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN EFF DATE IS PAST
      * JBT -> LSS WHEN EFF DATE IS PAST
      * LSS -> RDP WHEN EFF DATE IS PAST
      * RDP -> RPL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-REASON.
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
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WIRE TIER TIER FOR THE CORRESP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-TIER-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-CORRESP-AMT NOT < WS-WIRE-TIER-LOW(WS-TX)
                   MOVE WS-WIRE-TIER-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4010-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TLXM-CORRESP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-NOTICE.
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
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-QUEUED.
           IF TLXM-QUEUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-QUEUED-AMT * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-CHARGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E636 CONTACT REQUIRED
      * E510 WORK AMT 2 NEGATIVE
      * E911 WORK AMT 2 OVER 250000
      * E592 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN TLXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-EXPIRY.
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
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-EXPIRY.
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
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-NEXT-DUE.
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
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4080-FLAG-CHARGE.
           IF TLXM-CHARGE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E365 CONTACT REQUIRED
      * E957 WIRE AMT NEGATIVE
      * E721 WIRE AMT OVER 50000
      * E616 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT < 0
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT > 50000.00
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN TLXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * G -> M WHEN OPEN DATE IS PAST
      * M -> T WHEN OPEN DATE IS PAST
      * T -> G WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-REGION.
           EVALUATE TLXM-REGION-CD
               WHEN 'G'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE QUEUED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4110-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLXT-QUEUED-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLXT-QUEUED-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-EXPIRY.
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
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP FEE ON THE QUEUED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4130-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CORRESP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 3.00 TO TLXM-CORRESP-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 7.00 TO TLXM-CORRESP-AMT
               WHEN OTHER
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-CORRESP-AMT > 0 AND TLXM-CORRESP-AMT < 2.00
               MOVE 2.00 TO TLXM-CORRESP-AMT
           END-IF
           IF TLXM-CORRESP-AMT > 35.00
               MOVE 35.00 TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E760 REF NO REQUIRED
      * E507 WIRE AMT NEGATIVE
      * E243 WIRE AMT OVER 25000
      * E245 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-REF-NO = SPACES
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT < 0
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT > 25000.00
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E202 DESC REQUIRED
      * E603 SETTLE AMT NEGATIVE
      * E358 SETTLE AMT OVER 5000
      * E501 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT < 0
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT > 5000.00
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E501' TO WS-REJECT-CODE
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
       4160-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'OT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WIRE TIER TIER FOR THE COVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-COVER-AMT NOT < WS-WIRE-TIER-LOW(WS-TX)
                   MOVE WS-WIRE-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-CORRESP.
           IF TLXM-CORRESP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CORRESP-AMT * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'OT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-WIRE.
           IF TLXM-WIRE-AMT < 100.00
               MOVE 100.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-AMT > 1000.00
               MOVE 1000.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

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
                   MOVE TXCAL-CHARGE-AMT
                     TO TLXM-CHARGE-AMT
                   MOVE TXCAL-SETTLE-AMT
                     TO TLXM-SETTLE-AMT
                   MOVE TXCAL-COVER-YTD
                     TO TLXM-COVER-YTD
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
           DISPLAY 'TXXPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TXXPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
