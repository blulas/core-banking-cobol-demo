      *================================================================*
      * PROGRAM  : ATNRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ATM NETWORK SETTLEMENT - RECONCILIATION. READS THE  *
      *            ATN TRANSACTIONS, UPDATES THE ATN MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. PROPRIETARY SHARED ATM *
      *            NETWORK SETTLEMENT AND INTERCHANGE                  *
      * INPUT    : ATNMST (KSDS), ATNTRN, BANKCTL                      *
      * OUTPUT   : ATNOUT, ATNRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ATFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-28 JLM  ORIGINAL                                       *
      * 1998-02-27 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-06-06 MKL  ADJUST STATUS CODES ADDED INC4624              *
      * 2007-10-25 GAF  NEW DISPENSED REPORT COLUMNS CHG9491           *
      * 2008-08-22 TWH  AUDIT FINDING - DISPENSED CONTROL TOTALS CHG462*
      * 2010-12-07 SKN  AUDIT FINDING - SETTLE CONTROL TOTALS CHG5169  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATNRCON.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ATNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ATNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ATNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ATNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ATNRCRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ATNMST.
       FD  TRAN-FILE.
       COPY ATNTRN.
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
                                           VALUE 16.000000.
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
      * WS-CASSETTE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASSETTE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000870500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000631500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000813000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000037000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000678500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008750000000218000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009250000000280500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009350000000197000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009850000000040500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009900000000765500'.
       01  WS-CASSETTE-BAND-TBL REDEFINES WS-CASSETTE-BAND-VALUES.
           05  WS-CASSETTE-BAND-ROW OCCURS 10 TIMES.
               10  WS-CASSETTE-BAND-LOW    PIC 9(09)V99.
               10  WS-CASSETTE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ADJUST-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ADJUST-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000485500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000302500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000667000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000146000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000361000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013050000000665500'.
       01  WS-ADJUST-BAND-TBL REDEFINES WS-ADJUST-BAND-VALUES.
           05  WS-ADJUST-BAND-ROW OCCURS 8 TIMES.
               10  WS-ADJUST-BAND-LOW      PIC 9(09)V99.
               10  WS-ADJUST-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SURCHG-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURCHG-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000677000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000105000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000604500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000417500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000792500'.
       01  WS-SURCHG-TIER-TBL REDEFINES WS-SURCHG-TIER-VALUES.
           05  WS-SURCHG-TIER-ROW OCCURS 5 TIMES.
               10  WS-SURCHG-TIER-LOW      PIC 9(09)V99.
               10  WS-SURCHG-TIER-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ATNRCON  ATM NETWORK SETTLEMENT'.
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
       COPY ATFEEP.

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
               MOVE 'ATNMST' TO ABND-FILE-NAME
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
           MOVE ATNT-KEY TO ATNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-NETWORK
               PERFORM 3010-CHECK-NEXT-DUE
               PERFORM 3020-SELECT-RATE
               PERFORM 3030-SET-BRANCH
               PERFORM 3040-AGE-MATURITY
               PERFORM 3050-LIMIT-SURCHG
               PERFORM 3060-LIMIT-SETTLE
               PERFORM 3070-EDIT-MEMO
               PERFORM 3080-FLAG-SURCHG
               PERFORM 3090-AGE-EXPIRY
               PERFORM 3100-SELECT-DISCOUNT
               PERFORM 3110-AGE-MATURITY
               PERFORM 3120-AGE-NEXT-DUE
               PERFORM 3130-AGE-OPEN
               PERFORM 3140-ACCRUE-RATE
               PERFORM 3150-ACCUM-STATUS
               PERFORM 3160-LIMIT-SURCHG
               PERFORM 3170-CHECK-NOTICE
               PERFORM 3180-PRORATE-INTERCHG
               PERFORM 3190-PRORATE-ADJUST
               PERFORM 3200-FLAG-DISPENSED
               PERFORM 3210-SET-TERMINAL
               PERFORM 3220-ACCRUE-CASSETTE
               PERFORM 3230-PRORATE-ADJUST
               PERFORM 3240-COUNT-RETRY
               PERFORM 3250-SELECT-RATE
               PERFORM 3260-ASSESS-SUSPEND
               PERFORM 3270-EDIT-CUST-ID
               PERFORM 3280-EDIT-REJECT
               PERFORM 3290-DESCRIBE-REGION
               PERFORM 3300-AGE-EXPIRY
               PERFORM 3310-COUNT-PAYMENT
               PERFORM 3320-SPLIT-SURCHG
               PERFORM 3330-LIMIT-DISPENSED
               PERFORM 3340-ASSESS-STOP
               PERFORM 3350-PRORATE-INTERCHG
               PERFORM 3360-EDIT-NAME
               PERFORM 3370-ACCRUE-INTERCHG
               PERFORM 3380-SET-BRANCH
               PERFORM 3390-SET-TRAN
               PERFORM 3400-SET-BRANCH
               PERFORM 3410-FLAG-ADJUST
               PERFORM 3420-LIMIT-SURCHG
               PERFORM 3430-COUNT-ITEM
               PERFORM 3440-LIMIT-CASSETTE
               PERFORM 3450-SET-STATUS
               PERFORM 3460-ACCUM-BRANCH
               PERFORM 3470-SET-REASON
               PERFORM 3480-AGE-OPEN
               PERFORM 3490-FLAG-SURCHG
               PERFORM 3500-LIMIT-INTERCHG
               PERFORM 3510-COUNT-NOTICE
               PERFORM 3520-SPLIT-SURCHG
               PERFORM 3530-AGE-MATURITY
               PERFORM 3540-FLAG-ADJUST
               PERFORM 3550-ACCRUE-INTERCHG
               PERFORM 3560-COUNT-ITEM
               PERFORM 3570-ACCUM-TERMINAL
               PERFORM 3580-SET-NETWORK
               PERFORM 3590-SET-CHANNEL
               PERFORM 3600-FLAG-INTERCHG
               PERFORM 3610-COUNT-SEQ
               PERFORM 3620-AGE-NOTICE
               PERFORM 3630-COUNT-RETRY
               PERFORM 3640-DESCRIBE-STATUS
               PERFORM 3650-ASSESS-HOLD
               PERFORM 3660-CHECK-NEXT-DUE
               PERFORM 3670-CHECK-EFF
               PERFORM 3680-AGE-POST
               PERFORM 3690-ASSESS-EXEMPT
               PERFORM 3700-ASSESS-REVIEW
               PERFORM 3710-CHECK-NEXT-REVIEW
               PERFORM 3720-ACCUM-STATUS
               PERFORM 3730-LIMIT-INTERCHG
               PERFORM 3740-EDIT-REF-NO
               PERFORM 3750-CHECK-NEXT-REVIEW
               PERFORM 3760-SET-BRANCH
               PERFORM 3770-ASSESS-SUSPEND
               PERFORM 3780-FLAG-INTERCHG
               PERFORM 3790-SPLIT-ADJUST
               PERFORM 3800-EDIT-REJECT
               PERFORM 3810-CHECK-EXPIRY
               PERFORM 3820-SET-BRANCH
               PERFORM 3830-ACCRUE-SURCHG
               PERFORM 3840-SPLIT-CASSETTE
               PERFORM 3850-EDIT-NAME
               PERFORM 3860-PRORATE-SETTLE
               PERFORM 3870-FLAG-DISPENSED
               PERFORM 3880-CHECK-MATURITY
               PERFORM 3890-DESCRIBE-NETWORK
               PERFORM 3900-SET-BRANCH
               PERFORM 3910-COUNT-ITEM
               PERFORM 3920-FLAG-DISPENSED
               PERFORM 3930-PRORATE-DISPENSED
               PERFORM 3940-SPLIT-CASSETTE
               PERFORM 3950-ASSESS-REVIEW
               PERFORM 3960-ASSESS-SUSPEND
               PERFORM 3970-DESCRIBE-TERMINAL
               PERFORM 3980-ASSESS-STOP
               PERFORM 3990-ACCUM-STATUS
               PERFORM 4000-SET-BRANCH
               PERFORM 4010-SELECT-RATE
               PERFORM 4020-SPLIT-DISPENSED
               PERFORM 4030-CHECK-LAST-PAY
               PERFORM 4040-ASSESS-STOP
               PERFORM 4050-CHECK-NEXT-REVIEW
               PERFORM 4060-SPLIT-ADJUST
               PERFORM 4070-ACCUM-TERMINAL
               PERFORM 4080-ACCUM-BRANCH
               PERFORM 4090-FLAG-ITEM
               PERFORM 4100-SPLIT-SURCHG
               PERFORM 4110-CHECK-POST
               PERFORM 4120-SPLIT-DISPENSED
               PERFORM 7000-CALL-ATFEE
               REWRITE ATN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ATNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ATN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST BAND TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNT-DISPENSED-AMT NOT < WS-ADJUST-BAND-LOW(WS-TX)
                   MOVE WS-ADJUST-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN OPEN DATE IS PAST
      * HX -> YC WHEN OPEN DATE IS PAST
      * YC -> ZB WHEN OPEN DATE IS PAST
      * ZB -> DU WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-SURCHG.
           IF ATNM-SURCHG-AMT < 50.00
               MOVE 50.00 TO ATNM-SURCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-AMT > 5000.00
               MOVE 5000.00 TO ATNM-SURCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3060-LIMIT-SETTLE.
           IF ATNT-SETTLE-AMT < 50.00
               MOVE 50.00 TO ATNT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-SETTLE-AMT > 2500.00
               MOVE 2500.00 TO ATNT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E828 MEMO REQUIRED
      * E459 SURCHG AMT NEGATIVE
      * E161 SURCHG AMT OVER 25000
      * E849 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-MEMO = SPACES
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT < 0
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT > 25000.00
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN ATNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3080-FLAG-SURCHG.
           IF ATNM-SURCHG-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-SURCHG-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE CASSETTE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNT-CASSETTE-AMT NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ATNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATNM-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 50.00
               MOVE 50.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 5000.00
               MOVE 5000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTERCHG YTD.
      *----------------------------------------------------------------*
       3180-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-INTERCHG-YTD ROUNDED =
                   ATNT-SURCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-INTERCHG-YTD
           END-IF
           ADD ATNM-INTERCHG-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ADJUST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-AMT ROUNDED =
                   ATNM-ADJUST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3200-FLAG-DISPENSED.
           IF ATNM-DISPENSED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN OPEN DATE IS PAST
      * JJ -> JZ WHEN OPEN DATE IS PAST
      * JZ -> KK WHEN OPEN DATE IS PAST
      * KK -> BJ WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-CASSETTE.
           IF ATNM-CASSETTE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-CASSETTE-YTD * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADJUST AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-AMT ROUNDED =
                   ATNT-DISPENSED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE ADJUST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNM-ADJUST-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG YTD FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3260-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-YTD ROUNDED = WS-WORK-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 3.00 TO ATNM-SURCHG-YTD
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 5.00 TO ATNM-SURCHG-YTD
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-YTD ROUNDED = WS-WORK-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-SURCHG-YTD > 0 AND ATNM-SURCHG-YTD < 1.00
               MOVE 1.00 TO ATNM-SURCHG-YTD
           END-IF
           IF ATNM-SURCHG-YTD > 250.00
               MOVE 250.00 TO ATNM-SURCHG-YTD
           END-IF
           ADD ATNM-SURCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E864 CUST ID REQUIRED
      * E162 ADJUST AMT NEGATIVE
      * E801 ADJUST AMT OVER 99999
      * E906 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CUST-ID = SPACES
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT < 0
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT > 99999.00
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E137 REJECT CD REQUIRED
      * E013 DISPENSED AMT NEGATIVE
      * E984 DISPENSED AMT OVER 1000000
      * E241 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-REJECT-CD = SPACES
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT < 0
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT > 1000000.00
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN ATNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E241' TO WS-REJECT-CODE
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
       3290-DESCRIBE-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'V'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURCHG AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNT-SURCHG-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ATNT-SURCHG-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3330-LIMIT-DISPENSED.
           IF ATNM-DISPENSED-YTD < 10.00
               MOVE 10.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-DISPENSED-YTD > 10000.00
               MOVE 10000.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST FEE ON THE SURCHG YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3340-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-SURCHG-YTD *
                       ATNM-RATE / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO ATNM-ADJUST-AMT
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO ATNM-ADJUST-AMT
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-SURCHG-YTD *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-ADJUST-AMT > 0 AND ATNM-ADJUST-AMT < 5.00
               MOVE 5.00 TO ATNM-ADJUST-AMT
           END-IF
           IF ATNM-ADJUST-AMT > 25.00
               MOVE 25.00 TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3350-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-INTERCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E390 NAME REQUIRED
      * E649 WORK AMT NEGATIVE
      * E812 WORK AMT OVER 25000
      * E257 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E812' TO WS-REJECT-CODE
               WHEN ATNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-INTERCHG.
           IF ATNM-INTERCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-INTERCHG-AMT * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN MATURITY DATE IS PAST
      * HX -> YC WHEN MATURITY DATE IS PAST
      * YC -> ZB WHEN MATURITY DATE IS PAST
      * ZB -> DU WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN NEXT DUE DATE IS PAST
      * BI -> TR WHEN NEXT DUE DATE IS PAST
      * TR -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> RV WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN NEXT REVIEW DATE IS PAST
      * HX -> YC WHEN NEXT REVIEW DATE IS PAST
      * YC -> ZB WHEN NEXT REVIEW DATE IS PAST
      * ZB -> DU WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADJUST AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3410-FLAG-ADJUST.
           IF ATNM-ADJUST-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-ADJUST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3420-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 25.00
               MOVE 25.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 2500.00
               MOVE 2500.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 12
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-CASSETTE.
           IF ATNT-CASSETTE-AMT < 0.00
               MOVE 0.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-CASSETTE-AMT > 5000.00
               MOVE 5000.00 TO ATNT-CASSETTE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN EXPIRY DATE IS PAST
      * DP -> FJ WHEN EXPIRY DATE IS PAST
      * FJ -> NA WHEN EXPIRY DATE IS PAST
      * NA -> DF WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNM-ADJUST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-ADJUST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN POST DATE IS PAST
      * HX -> PR WHEN POST DATE IS PAST
      * PR -> XD WHEN POST DATE IS PAST
      * XD -> ZJ WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3490-FLAG-SURCHG.
           IF ATNT-SURCHG-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNT-SURCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-INTERCHG.
           IF ATNM-INTERCHG-AMT < 50.00
               MOVE 50.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-INTERCHG-AMT > 1000.00
               MOVE 1000.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO ATNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURCHG YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNM-SURCHG-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ATNM-SURCHG-YTD - (WS-WORK-AMT * 2)
           COMPUTE ATNM-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADJUST YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-ADJUST.
           IF ATNM-ADJUST-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-ADJUST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-INTERCHG.
           IF ATNM-INTERCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-INTERCHG-AMT * ATNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 12
               MOVE 'Y' TO ATNM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNM-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN EXPIRY DATE IS PAST
      * C -> H WHEN EXPIRY DATE IS PAST
      * H -> R WHEN EXPIRY DATE IS PAST
      * R -> S WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTERCHG YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-INTERCHG.
           IF ATNM-INTERCHG-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-INTERCHG-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 10
               MOVE 'Y' TO ATNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3620-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASSETTE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3650-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ATNM-CASSETTE-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ATNM-CASSETTE-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ATNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DISPENSED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3690-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNM-DISPENSED-YTD *
                       ATNM-FEE-PCT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNM-DISPENSED-YTD *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3700-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = WS-WORK-AMT-2 *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-BRANCH-CD = 'DU'
                   MOVE 15.00 TO ATNM-ADJUST-YTD
               WHEN ATNM-BRANCH-CD = 'HX'
                   MOVE 10.00 TO ATNM-ADJUST-YTD
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = WS-WORK-AMT-2 *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-ADJUST-YTD > 0 AND ATNM-ADJUST-YTD < 1.00
               MOVE 1.00 TO ATNM-ADJUST-YTD
           END-IF
           IF ATNM-ADJUST-YTD > 50.00
               MOVE 50.00 TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-INTERCHG.
           IF ATNM-INTERCHG-AMT < 50.00
               MOVE 50.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-INTERCHG-AMT > 50000.00
               MOVE 50000.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 REF NO REQUIRED
      * E705 SETTLE AMT NEGATIVE
      * E070 SETTLE AMT OVER 5000
      * E423 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-REF-NO = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT < 0
                   MOVE 'E705' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-AMT > 5000.00
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E423' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN LAST PAY DATE IS PAST
      * HX -> YC WHEN LAST PAY DATE IS PAST
      * YC -> ZB WHEN LAST PAY DATE IS PAST
      * ZB -> DU WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG FEE ON THE DISPENSED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3770-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-FEE-PCT / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 12.00 TO ATNM-INTERCHG-AMT
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 20.00 TO ATNM-INTERCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-INTERCHG-AMT > 0 AND ATNM-INTERCHG-AMT < 10.00
               MOVE 10.00 TO ATNM-INTERCHG-AMT
           END-IF
           IF ATNM-INTERCHG-AMT > 100.00
               MOVE 100.00 TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INTERCHG AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3780-FLAG-INTERCHG.
           IF ATNM-INTERCHG-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-INTERCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-YTD - (WS-WORK-AMT * 4)
           COMPUTE ATNM-SURCHG-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E992 REJECT CD REQUIRED
      * E449 SURCHG YTD NEGATIVE
      * E435 SURCHG YTD OVER 1000000
      * E005 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-REJECT-CD = SPACES
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD < 0
                   MOVE 'E449' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD > 1000000.00
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN EFFECTIVE DATE IS PAST
      * HX -> YC WHEN EFFECTIVE DATE IS PAST
      * YC -> ZB WHEN EFFECTIVE DATE IS PAST
      * ZB -> DU WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-SURCHG.
           IF ATNM-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SURCHG-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-CASSETTE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASSETTE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-CASSETTE.
           COMPUTE WS-WORK-AMT = ATNM-CASSETTE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-CASSETTE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ATNT-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E525 NAME REQUIRED
      * E934 ADJUST YTD NEGATIVE
      * E788 ADJUST YTD OVER 5000
      * E084 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-YTD < 0
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-YTD > 5000.00
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN ATNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-DISPENSED.
           IF ATNM-DISPENSED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN MATURITY DATE IS PAST
      * HX -> YC WHEN MATURITY DATE IS PAST
      * YC -> ZB WHEN MATURITY DATE IS PAST
      * ZB -> DU WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 5
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3920-FLAG-DISPENSED.
           IF ATNM-DISPENSED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADJUST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISPENSED YTD.
      *----------------------------------------------------------------*
       3930-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-DISPENSED-YTD ROUNDED =
                   ATNM-ADJUST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-DISPENSED-YTD
           END-IF
           ADD ATNM-DISPENSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-CASSETTE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE ATNT-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DISPENSED YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3950-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNM-DISPENSED-YTD *
                       ATNM-RATE / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ATNM-DISPENSED-YTD *
                       ATNM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE YTD FEE ON THE SURCHG AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3960-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-YTD ROUNDED = ATNM-SURCHG-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 20.00 TO ATNM-CASSETTE-YTD
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 20.00 TO ATNM-CASSETTE-YTD
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-YTD ROUNDED = ATNM-SURCHG-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-CASSETTE-YTD > 0 AND ATNM-CASSETTE-YTD < 2.00
               MOVE 2.00 TO ATNM-CASSETTE-YTD
           END-IF
           IF ATNM-CASSETTE-YTD > 75.00
               MOVE 75.00 TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE DISPENSED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3980-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED =
                       ATNT-DISPENSED-AMT * ATNM-RATE / 200
               WHEN ATNM-REASON-CD = 'AK'
                   MOVE 10.00 TO ATNM-CASSETTE-AMT
               WHEN ATNM-REASON-CD = 'HX'
                   MOVE 15.00 TO ATNM-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED =
                       ATNT-DISPENSED-AMT * ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-CASSETTE-AMT > 0 AND ATNM-CASSETTE-AMT < 1.00
               MOVE 1.00 TO ATNM-CASSETTE-AMT
           END-IF
           IF ATNM-CASSETTE-AMT > 250.00
               MOVE 250.00 TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNM-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN CLOSE DATE IS PAST
      * HX -> YC WHEN CLOSE DATE IS PAST
      * YC -> ZB WHEN CLOSE DATE IS PAST
      * ZB -> DU WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASSETTE BAND TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASSETTE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNM-SETTLE-YTD NOT < WS-CASSETTE-BAND-LOW(WS-TX)
                   MOVE WS-CASSETTE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNT-DISPENSED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ATNT-DISPENSED-AMT - (WS-WORK-AMT *
               4)
           COMPUTE ATNM-ADJUST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE DISPENSED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4040-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNT-DISPENSED-AMT
                       * ATNM-FEE-PCT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO ATNM-SURCHG-AMT
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO ATNM-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNT-DISPENSED-AMT
                       * ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-SURCHG-AMT > 0 AND ATNM-SURCHG-AMT < 5.00
               MOVE 5.00 TO ATNM-SURCHG-AMT
           END-IF
           IF ATNM-SURCHG-AMT > 50.00
               MOVE 50.00 TO ATNM-SURCHG-AMT
           END-IF
           ADD ATNM-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4050-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNM-DISPENSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG YTD BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNM-SURCHG-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SURCHG-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4090-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURCHG AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNM-SURCHG-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SURCHG-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNM-DISPENSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ATNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ATN FIELDS TO ATFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-ATFEE.
           INITIALIZE ATFEE-PARMS
           MOVE ATNM-DISPENSED-YTD
             TO ATFEE-DISPENSED-YTD
           MOVE ATNM-SETTLE-YTD
             TO ATFEE-SETTLE-YTD
           MOVE ATNM-ADJUST-YTD
             TO ATFEE-ADJUST-YTD
           MOVE ATNM-CASSETTE-AMT
             TO ATFEE-CASSETTE-AMT
           MOVE ATNM-SETTLE-AMT
             TO ATFEE-SETTLE-AMT
           MOVE ATNM-INTERCHG-YTD
             TO ATFEE-INTERCHG-YTD
           MOVE ATNM-OPEN-DATE
             TO ATFEE-OPEN-DATE
           MOVE ATNM-MATURITY-DATE
             TO ATFEE-MATURITY-DATE
           MOVE ATNM-NOTICE-DATE
             TO ATFEE-NOTICE-DATE
           MOVE ATNM-LAST-PAY-DATE
             TO ATFEE-LAST-PAY-DATE
           MOVE ATNM-STATUS-CD
             TO ATFEE-STATUS-CD
           MOVE ATNM-REGION-CD
             TO ATFEE-REGION-CD
           MOVE ATNM-TERMINAL-CD
             TO ATFEE-TERMINAL-CD
           MOVE ATNM-BRANCH-CD
             TO ATFEE-BRANCH-CD
           MOVE ATNM-REVIEW-FLAG
             TO ATFEE-REVIEW-FLAG
           MOVE ATNM-STOP-FLAG
             TO ATFEE-STOP-FLAG
           MOVE ATNM-EXEMPT-FLAG
             TO ATFEE-EXEMPT-FLAG
           MOVE ATNM-RETRY-CNT
             TO ATFEE-RETRY-CNT
           MOVE ATNM-NOTICE-CNT
             TO ATFEE-NOTICE-CNT
           MOVE ATNM-DISCOUNT
             TO ATFEE-DISCOUNT
           MOVE ATNM-FEE-PCT
             TO ATFEE-FEE-PCT
           MOVE ATNM-ADDR
             TO ATFEE-ADDR
           MOVE ATNM-REF-NO
             TO ATFEE-REF-NO
           CALL 'ATFEE' USING ATFEE-PARMS
           EVALUATE ATFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ATFEE-SETTLE-AMT
                     TO ATNM-SETTLE-AMT
                   MOVE ATFEE-INTERCHG-YTD
                     TO ATNM-INTERCHG-YTD
                   MOVE ATFEE-REVIEW-FLAG
                     TO ATNM-REVIEW-FLAG
                   MOVE ATFEE-STOP-FLAG
                     TO ATNM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ATFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ATNT-KEY TO RD-KEY
           MOVE ATNT-TRAN-CD TO RD-CODE
           MOVE ATNT-SURCHG-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ATNT-KEY TO RD-KEY
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
           DISPLAY 'ATNRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ATNRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
