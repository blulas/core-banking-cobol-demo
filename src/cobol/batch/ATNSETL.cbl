      *================================================================*
      * PROGRAM  : ATNSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ATM NETWORK SETTLEMENT - SETTLEMENT. READS THE ATN  *
      *            TRANSACTIONS, UPDATES THE ATN MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PROPRIETARY SHARED ATM NETWORK  *
      *            SETTLEMENT AND INTERCHANGE                          *
      * INPUT    : ATNMST (KSDS), ATNTRN, BANKCTL                      *
      * OUTPUT   : ATNOUT, ATNSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ATWAIV                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-02 SKN  ORIGINAL                                       *
      * 1992-01-26 CRP  NEW ADJUST REPORT COLUMNS CHG6190              *
      * 1993-11-23 GAF  CASSETTE EXCEPTIONS TO OPS QUEUE PRB7893       *
      * 1993-07-11 JLM  CASSETTE STATUS CODES ADDED REQ9124            *
      * 1994-02-16 JLM  NEW ADJUST REPORT COLUMNS INC5000              *
      * 2007-12-27 CRP  CASSETTE NOTICE TEXT PER LEGAL CHG4162         *
      * 2009-03-05 EWS  SURCHG EXCEPTIONS TO OPS QUEUE CHG0288         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATNSETL.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "ATNSERPT"
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
                                           VALUE 23.000000.
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
      * WS-SETTLE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000541000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000625500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000233000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000459000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005200000000384000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005700000000039000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006700000000503000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007700000000200000'.
       01  WS-SETTLE-BAND-TBL REDEFINES WS-SETTLE-BAND-VALUES.
           05  WS-SETTLE-BAND-ROW OCCURS 8 TIMES.
               10  WS-SETTLE-BAND-LOW      PIC 9(09)V99.
               10  WS-SETTLE-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SETTLE-BANDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-BANDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000078000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000381000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000075000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000220000'.
       01  WS-SETTLE-BANDX-TBL REDEFINES WS-SETTLE-BANDX-VALUES.
           05  WS-SETTLE-BANDX-ROW OCCURS 4 TIMES.
               10  WS-SETTLE-BANDX-LOW     PIC 9(09)V99.
               10  WS-SETTLE-BANDX-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ATNSETL  ATM NETWORK SETTLEMENT'.
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
       COPY ATWAIVP.

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
               PERFORM 3000-EDIT-ADDR
               PERFORM 3010-ASSESS-HOLD
               PERFORM 3020-PRORATE-CASSETTE
               PERFORM 3030-ACCUM-STATUS
               PERFORM 3040-SET-CHANNEL
               PERFORM 3050-ACCUM-REASON
               PERFORM 3060-EDIT-OPERATOR
               PERFORM 3070-SELECT-DISCOUNT
               PERFORM 3080-ACCUM-STATUS
               PERFORM 3090-EDIT-DESC
               PERFORM 3100-AGE-MATURITY
               PERFORM 3110-SET-REGION
               PERFORM 3120-CHECK-POST
               PERFORM 3130-COUNT-RETRY
               PERFORM 3140-CHECK-EXPIRY
               PERFORM 3150-DESCRIBE-CHANNEL
               PERFORM 3160-SELECT-FEE-PCT
               PERFORM 3170-LIMIT-CASSETTE
               PERFORM 3180-DESCRIBE-REASON
               PERFORM 3190-DESCRIBE-TERMINAL
               PERFORM 3200-COUNT-SEQ
               PERFORM 3210-COUNT-PAYMENT
               PERFORM 3220-SET-TERMINAL
               PERFORM 3230-ASSESS-EXEMPT
               PERFORM 3240-ASSESS-STOP
               PERFORM 3250-DESCRIBE-NETWORK
               PERFORM 3260-ACCUM-CHANNEL
               PERFORM 3270-SELECT-DISCOUNT
               PERFORM 3280-DESCRIBE-TERMINAL
               PERFORM 3290-SPLIT-INTERCHG
               PERFORM 3300-LIMIT-SETTLE
               PERFORM 3310-ACCUM-BRANCH
               PERFORM 3320-SET-STATUS
               PERFORM 3330-CHECK-CLOSE
               PERFORM 3340-COUNT-RETRY
               PERFORM 3350-CHECK-NEXT-REVIEW
               PERFORM 3360-PRORATE-CASSETTE
               PERFORM 3370-LIMIT-INTERCHG
               PERFORM 3380-SPLIT-SURCHG
               PERFORM 3390-EDIT-REF-NO
               PERFORM 3400-AGE-NEXT-DUE
               PERFORM 3410-ASSESS-NOTICE-SENT
               PERFORM 3420-LIMIT-SETTLE
               PERFORM 3430-SELECT-RATE
               PERFORM 3440-FLAG-ITEM
               PERFORM 3450-EDIT-CONTACT
               PERFORM 3460-AGE-CLOSE
               PERFORM 3470-COUNT-NOTICE
               PERFORM 3480-DESCRIBE-STATUS
               PERFORM 3490-PRORATE-SURCHG
               PERFORM 3500-LIMIT-SURCHG
               PERFORM 3510-COUNT-ITEM
               PERFORM 3520-DESCRIBE-REASON
               PERFORM 3530-SET-REASON
               PERFORM 3540-SELECT-RATE
               PERFORM 3550-ASSESS-NOTICE-SENT
               PERFORM 3560-ACCRUE-SETTLE
               PERFORM 3570-DESCRIBE-TRAN
               PERFORM 3580-FLAG-CASSETTE
               PERFORM 3590-SET-NETWORK
               PERFORM 3600-AGE-OPEN
               PERFORM 3610-DESCRIBE-REGION
               PERFORM 3620-CHECK-EFFECTIVE
               PERFORM 3630-DESCRIBE-CHANNEL
               PERFORM 3640-EDIT-ADDR
               PERFORM 3650-EDIT-ADDR
               PERFORM 3660-ASSESS-REVIEW
               PERFORM 3670-CHECK-POST
               PERFORM 3680-EDIT-OPERATOR
               PERFORM 3690-PRORATE-INTERCHG
               PERFORM 3700-CHECK-CLOSE
               PERFORM 3710-SPLIT-SETTLE
               PERFORM 3720-CHECK-MATURITY
               PERFORM 3730-EDIT-DESC
               PERFORM 3740-PRORATE-SURCHG
               PERFORM 3750-COUNT-ITEM
               PERFORM 3760-SET-TERMINAL
               PERFORM 3770-ACCUM-TRAN
               PERFORM 3780-CHECK-LAST-PAY
               PERFORM 3790-SPLIT-DISPENSED
               PERFORM 3800-FLAG-DISPENSED
               PERFORM 3810-FLAG-ADJUST
               PERFORM 3820-EDIT-OPERATOR
               PERFORM 3830-SELECT-DISCOUNT
               PERFORM 3840-COUNT-PAYMENT
               PERFORM 3850-ASSESS-HOLD
               PERFORM 3860-ASSESS-STOP
               PERFORM 3870-SET-REGION
               PERFORM 3880-CHECK-NOTICE
               PERFORM 3890-SET-NETWORK
               PERFORM 3900-ACCRUE-CASSETTE
               PERFORM 3910-COUNT-RETRY
               PERFORM 3920-SET-REGION
               PERFORM 3930-ACCRUE-SETTLE
               PERFORM 3940-ASSESS-EXEMPT
               PERFORM 3950-SELECT-RATE
               PERFORM 3960-SPLIT-DISPENSED
               PERFORM 3970-FLAG-CASSETTE
               PERFORM 3980-SPLIT-SURCHG
               PERFORM 3990-ACCUM-TRAN
               PERFORM 4000-CHECK-EFFECTIVE
               PERFORM 4010-LIMIT-DISPENSED
               PERFORM 4020-SET-TERMINAL
               PERFORM 4030-SELECT-RATE
               PERFORM 4040-SELECT-RATE
               PERFORM 4050-ACCUM-TERMINAL
               PERFORM 4060-ASSESS-REVIEW
               PERFORM 4070-ACCUM-REGION
               PERFORM 4080-DESCRIBE-BRANCH
               PERFORM 4090-LIMIT-INTERCHG
               PERFORM 4100-ASSESS-HOLD
               PERFORM 4110-SET-CHANNEL
               PERFORM 4120-CHECK-OPEN
               PERFORM 4130-ASSESS-NOTICE-SENT
               PERFORM 4140-CHECK-MATURITY
               PERFORM 4150-FLAG-SETTLE
               PERFORM 4160-DESCRIBE-CHANNEL
               PERFORM 4170-LIMIT-SURCHG
               PERFORM 4180-ASSESS-NOTICE-SENT
               PERFORM 4190-LIMIT-CASSETTE
               PERFORM 4200-ACCUM-NETWORK
               PERFORM 4210-EDIT-CUST-ID
               PERFORM 4220-AGE-NEXT-DUE
               PERFORM 4230-PRORATE-INTERCHG
               PERFORM 4240-SELECT-DISCOUNT
               PERFORM 4250-SET-REGION
               PERFORM 4260-SPLIT-CASSETTE
               PERFORM 4270-SPLIT-SURCHG
               PERFORM 4280-COUNT-RETRY
               PERFORM 4290-DESCRIBE-STATUS
               PERFORM 4300-CHECK-EFF
               PERFORM 4310-ASSESS-EXEMPT
               PERFORM 4320-LIMIT-ADJUST
               PERFORM 4330-FLAG-SURCHG
               PERFORM 4340-AGE-EFFECTIVE
               PERFORM 4350-SELECT-RATE
               PERFORM 4360-SELECT-RATE
               PERFORM 4370-PRORATE-CASSETTE
               PERFORM 4380-CHECK-NEXT-REVIEW
               PERFORM 4390-AGE-CLOSE
               PERFORM 4400-CHECK-NEXT-REVIEW
               PERFORM 4410-SPLIT-INTERCHG
               PERFORM 4420-SET-TERMINAL
               PERFORM 4430-ACCRUE-SETTLE
               PERFORM 4440-ACCUM-CHANNEL
               PERFORM 4450-FLAG-SETTLE
               PERFORM 4460-PRORATE-ADJUST
               PERFORM 4470-AGE-POST
               PERFORM 4480-EDIT-MEMO
               PERFORM 4490-EDIT-ADDR
               PERFORM 4500-DESCRIBE-NETWORK
               PERFORM 4510-SELECT-FEE-PCT
               PERFORM 4520-SELECT-DISCOUNT
               PERFORM 4530-AGE-OPEN
               PERFORM 4540-ACCRUE-SURCHG
               PERFORM 4550-FLAG-DISPENSED
               PERFORM 4560-SET-BRANCH
               PERFORM 4570-LIMIT-DISPENSED
               PERFORM 4580-DESCRIBE-BRANCH
               PERFORM 4590-SPLIT-INTERCHG
               PERFORM 4600-COUNT-NOTICE
               PERFORM 4610-SPLIT-SETTLE
               PERFORM 4620-SELECT-DISCOUNT
               PERFORM 4630-SET-REASON
               PERFORM 4640-FLAG-ADJUST
               PERFORM 4650-ASSESS-REVERSAL
               PERFORM 4660-SPLIT-SETTLE
               PERFORM 4670-CHECK-NOTICE
               PERFORM 4680-COUNT-SEQ
               PERFORM 4690-AGE-EXPIRY
               PERFORM 4700-SET-REASON
               PERFORM 4710-EDIT-ADDR
               PERFORM 4720-FLAG-CASSETTE
               PERFORM 4730-DESCRIBE-NETWORK
               PERFORM 4740-PRORATE-CASSETTE
               PERFORM 4750-LIMIT-DISPENSED
               PERFORM 4760-ASSESS-HOLD
               PERFORM 4770-PRORATE-CASSETTE
               PERFORM 4780-ASSESS-EXEMPT
               PERFORM 4790-COUNT-ITEM
               PERFORM 7010-CALL-ATWAIV
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E976 ADDR REQUIRED
      * E179 ADJUST YTD NEGATIVE
      * E332 ADJUST YTD OVER 50000
      * E748 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-YTD < 0
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-YTD > 50000.00
                   MOVE 'E332' TO WS-REJECT-CODE
               WHEN ATNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E748' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE CASSETTE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3010-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-SETTLE-YTD ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-RATE / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 5.00 TO ATNM-SETTLE-YTD
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 15.00 TO ATNM-SETTLE-YTD
               WHEN OTHER
                   COMPUTE ATNM-SETTLE-YTD ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-SETTLE-YTD > 0 AND ATNM-SETTLE-YTD < 2.00
               MOVE 2.00 TO ATNM-SETTLE-YTD
           END-IF
           IF ATNM-SETTLE-YTD > 50.00
               MOVE 50.00 TO ATNM-SETTLE-YTD
           END-IF
           ADD ATNM-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASSETTE YTD.
      *----------------------------------------------------------------*
       3020-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-YTD ROUNDED =
                   ATNT-DISPENSED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-STATUS.
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
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REASON-CD
                   ADD ATNT-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E153 OPERATOR REQUIRED
      * E091 WORK AMT NEGATIVE
      * E232 WORK AMT OVER 250000
      * E926 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-OPERATOR = SPACES
                   MOVE 'E153' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN ATNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE SURCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNM-SURCHG-YTD NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-STATUS-CD
                   ADD ATNM-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E710 DESC REQUIRED
      * E982 DISPENSED AMT NEGATIVE
      * E175 DISPENSED AMT OVER 50000
      * E058 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E710' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT < 0
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT > 50000.00
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-MATURITY.
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
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN NOTICE DATE IS PAST
      * G -> H WHEN NOTICE DATE IS PAST
      * H -> P WHEN NOTICE DATE IS PAST
      * P -> V WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-POST.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ATNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-EXPIRY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BANDX TIER FOR THE SURCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BANDX-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-SURCHG-AMT NOT < WS-SETTLE-BANDX-LOW(WS-TX)
                   MOVE WS-SETTLE-BANDX-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-CASSETTE.
           IF ATNM-CASSETTE-AMT < 10.00
               MOVE 10.00 TO ATNM-CASSETTE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-CASSETTE-AMT > 1000.00
               MOVE 1000.00 TO ATNM-CASSETTE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 6
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO ATNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN POST DATE IS PAST
      * JJ -> JZ WHEN POST DATE IS PAST
      * JZ -> KK WHEN POST DATE IS PAST
      * KK -> BJ WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE CASSETTE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-REASON-CD = 'AK'
                   MOVE 3.00 TO ATNM-SETTLE-AMT
               WHEN ATNM-REASON-CD = 'HX'
                   MOVE 20.00 TO ATNM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-SETTLE-AMT > 0 AND ATNM-SETTLE-AMT < 5.00
               MOVE 5.00 TO ATNM-SETTLE-AMT
           END-IF
           IF ATNM-SETTLE-AMT > 100.00
               MOVE 100.00 TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST YTD FEE ON THE SURCHG AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3240-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO ATNM-ADJUST-YTD
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO ATNM-ADJUST-YTD
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-ADJUST-YTD > 0 AND ATNM-ADJUST-YTD < 5.00
               MOVE 5.00 TO ATNM-ADJUST-YTD
           END-IF
           IF ATNM-ADJUST-YTD > 75.00
               MOVE 75.00 TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-CHANNEL-CD
                   ADD ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BANDX TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BANDX-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-INTERCHG-YTD NOT < WS-SETTLE-BANDX-LOW(WS-TX)
                   MOVE WS-SETTLE-BANDX-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3290-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATNM-INTERCHG-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-INTERCHG-AMT - (WS-WORK-AMT * 4)
           COMPUTE ATNT-DISPENSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3300-LIMIT-SETTLE.
           IF ATNT-SETTLE-AMT < 25.00
               MOVE 25.00 TO ATNT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-SETTLE-AMT > 2500.00
               MOVE 2500.00 TO ATNT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DF -> DP WHEN NEXT REVIEW DATE IS PAST
      * DP -> FJ WHEN NEXT REVIEW DATE IS PAST
      * FJ -> NA WHEN NEXT REVIEW DATE IS PAST
      * NA -> DF WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DP' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJ'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NA' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NA'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DF' TO ATNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
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
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASSETTE YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-YTD ROUNDED =
                   ATNM-ADJUST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INTERCHG YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-INTERCHG.
           IF ATNM-INTERCHG-YTD < 100.00
               MOVE 100.00 TO ATNM-INTERCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-INTERCHG-YTD > 10000.00
               MOVE 10000.00 TO ATNM-INTERCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNM-SURCHG-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E717 REF NO REQUIRED
      * E479 DISPENSED AMT NEGATIVE
      * E045 DISPENSED AMT OVER 250000
      * E485 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-REF-NO = SPACES
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT < 0
                   MOVE 'E479' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT > 250000.00
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN ATNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-NEXT-DUE.
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
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3410-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-YTD ROUNDED = WS-WORK-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 10.00 TO ATNM-SURCHG-YTD
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 15.00 TO ATNM-SURCHG-YTD
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-YTD ROUNDED = WS-WORK-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-SURCHG-YTD > 0 AND ATNM-SURCHG-YTD < 5.00
               MOVE 5.00 TO ATNM-SURCHG-YTD
           END-IF
           IF ATNM-SURCHG-YTD > 250.00
               MOVE 250.00 TO ATNM-SURCHG-YTD
           END-IF
           ADD ATNM-SURCHG-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-SETTLE.
           IF ATNT-SETTLE-AMT < 50.00
               MOVE 50.00 TO ATNT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-SETTLE-AMT > 5000.00
               MOVE 5000.00 TO ATNT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BANDX TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3430-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BANDX-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNT-DISPENSED-AMT NOT < WS-SETTLE-BANDX-LOW(WS-TX)
                   MOVE WS-SETTLE-BANDX-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3440-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E363 CONTACT REQUIRED
      * E437 SURCHG AMT NEGATIVE
      * E564 SURCHG AMT OVER 10000
      * E829 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT < 0
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT > 10000.00
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN ATNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURCHG YTD.
      *----------------------------------------------------------------*
       3490-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SURCHG-YTD ROUNDED =
                   ATNT-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SURCHG-YTD
           END-IF
           ADD ATNM-SURCHG-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SURCHG AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-SURCHG.
           IF ATNM-SURCHG-AMT < 50.00
               MOVE 50.00 TO ATNM-SURCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-AMT > 50000.00
               MOVE 50000.00 TO ATNM-SURCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 5
               MOVE 'Y' TO ATNM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN NOTICE DATE IS PAST
      * HX -> PR WHEN NOTICE DATE IS PAST
      * PR -> XD WHEN NOTICE DATE IS PAST
      * XD -> ZJ WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BANDX TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BANDX-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-SETTLE-BANDX-LOW(WS-TX)
                   MOVE WS-SETTLE-BANDX-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3550-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = WS-WORK-AMT-2 *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 5.00 TO ATNM-ADJUST-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 12.00 TO ATNM-ADJUST-AMT
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = WS-WORK-AMT-2 *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-ADJUST-AMT > 0 AND ATNM-ADJUST-AMT < 1.00
               MOVE 1.00 TO ATNM-ADJUST-AMT
           END-IF
           IF ATNM-ADJUST-AMT > 150.00
               MOVE 150.00 TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-SETTLE.
           IF ATNT-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SETTLE-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-DISPENSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BI'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASSETTE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3580-FLAG-CASSETTE.
           IF ATNM-CASSETTE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN EXPIRY DATE IS PAST
      * C -> H WHEN EXPIRY DATE IS PAST
      * H -> R WHEN EXPIRY DATE IS PAST
      * R -> S WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-NETWORK.
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
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-OPEN.
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
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-SURCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E275 ADDR REQUIRED
      * E135 INTERCHG YTD NEGATIVE
      * E014 INTERCHG YTD OVER 25000
      * E767 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN ATNM-INTERCHG-YTD < 0
                   MOVE 'E135' TO WS-REJECT-CODE
               WHEN ATNM-INTERCHG-YTD > 25000.00
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN ATNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E716 ADDR REQUIRED
      * E773 SURCHG YTD NEGATIVE
      * E253 SURCHG YTD OVER 250000
      * E272 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD < 0
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-YTD > 250000.00
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN ATNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST FEE ON THE INTERCHG AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3660-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 5.00 TO ATNM-ADJUST-AMT
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 15.00 TO ATNM-ADJUST-AMT
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-ADJUST-AMT > 0 AND ATNM-ADJUST-AMT < 5.00
               MOVE 5.00 TO ATNM-ADJUST-AMT
           END-IF
           IF ATNM-ADJUST-AMT > 100.00
               MOVE 100.00 TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-POST.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E372 OPERATOR REQUIRED
      * E586 SURCHG AMT NEGATIVE
      * E968 SURCHG AMT OVER 1000000
      * E367 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-OPERATOR = SPACES
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT < 0
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT > 1000000.00
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN ATNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTERCHG AMT.
      *----------------------------------------------------------------*
       3690-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-INTERCHG-AMT ROUNDED =
                   ATNM-INTERCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE ATNM-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-MATURITY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E849 DESC REQUIRED
      * E628 DISPENSED YTD NEGATIVE
      * E969 DISPENSED YTD OVER 10000
      * E977 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-YTD < 0
                   MOVE 'E628' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-YTD > 10000.00
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURCHG YTD.
      *----------------------------------------------------------------*
       3740-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SURCHG-YTD ROUNDED =
                   ATNM-SETTLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-SURCHG-YTD
           END-IF
           ADD ATNM-SURCHG-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 12
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN EFF DATE IS PAST
      * JJ -> JZ WHEN EFF DATE IS PAST
      * JZ -> KK WHEN EFF DATE IS PAST
      * KK -> BJ WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-TRAN-CD
                   ADD ATNM-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-LAST-PAY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-DISPENSED.
           IF ATNM-DISPENSED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3810-FLAG-ADJUST.
           IF ATNM-ADJUST-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-ADJUST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E730 OPERATOR REQUIRED
      * E015 DISPENSED YTD NEGATIVE
      * E145 DISPENSED YTD OVER 5000
      * E011 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-OPERATOR = SPACES
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-YTD < 0
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-YTD > 5000.00
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN ATNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNM-INTERCHG-YTD NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST YTD FEE ON THE ADJUST AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3850-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNM-ADJUST-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 5.00 TO ATNM-ADJUST-YTD
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 15.00 TO ATNM-ADJUST-YTD
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNM-ADJUST-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-ADJUST-YTD > 0 AND ATNM-ADJUST-YTD < 2.00
               MOVE 2.00 TO ATNM-ADJUST-YTD
           END-IF
           IF ATNM-ADJUST-YTD > 25.00
               MOVE 25.00 TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE DISPENSED YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3860-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNM-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-DISCOUNT / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 5.00 TO ATNM-DISPENSED-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 15.00 TO ATNM-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-DISPENSED-AMT > 0 AND ATNM-DISPENSED-AMT < 2.00
               MOVE 2.00 TO ATNM-DISPENSED-AMT
           END-IF
           IF ATNM-DISPENSED-AMT > 75.00
               MOVE 75.00 TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN CLOSE DATE IS PAST
      * G -> H WHEN CLOSE DATE IS PAST
      * H -> P WHEN CLOSE DATE IS PAST
      * P -> V WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-NOTICE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN OPEN DATE IS PAST
      * C -> H WHEN OPEN DATE IS PAST
      * H -> R WHEN OPEN DATE IS PAST
      * R -> S WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-CASSETTE.
           IF ATNM-CASSETTE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-CASSETTE-AMT * ATNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ATNT-DISPENSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN EFF DATE IS PAST
      * G -> H WHEN EFF DATE IS PAST
      * H -> P WHEN EFF DATE IS PAST
      * P -> V WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-SETTLE.
           IF ATNT-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SETTLE-AMT * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE INTERCHG YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3940-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-INTERCHG-YTD *
                       ATNM-RATE / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 15.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 3.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-INTERCHG-YTD *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 2.00
               MOVE 2.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 250.00
               MOVE 250.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BANDX TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BANDX-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-DISPENSED-AMT NOT < WS-SETTLE-BANDX-LOW(WS-TX)
                   MOVE WS-SETTLE-BANDX-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISPENSED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-DISPENSED.
           COMPUTE WS-WORK-AMT = ATNM-DISPENSED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-DISPENSED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE ATNT-SURCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3970-FLAG-CASSETTE.
           IF ATNT-CASSETTE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNT-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURCHG AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNM-SURCHG-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SURCHG-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNM-INTERCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTERCHG AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-TRAN-CD
                   ADD ATNM-INTERCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-INTERCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ATNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4010-LIMIT-DISPENSED.
           IF ATNT-DISPENSED-AMT < 25.00
               MOVE 25.00 TO ATNT-DISPENSED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-DISPENSED-AMT > 500.00
               MOVE 500.00 TO ATNT-DISPENSED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN OPEN DATE IS PAST
      * JJ -> JZ WHEN OPEN DATE IS PAST
      * JZ -> KK WHEN OPEN DATE IS PAST
      * KK -> BJ WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-TERMINAL.
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
      * SELECT THE SETTLE BAND TIER FOR THE ADJUST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNM-ADJUST-YTD NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BANDX TIER FOR THE ADJUST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BANDX-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-ADJUST-YTD NOT < WS-SETTLE-BANDX-LOW(WS-TX)
                   MOVE WS-SETTLE-BANDX-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4060-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATNM-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED = WS-WORK-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 10.00 TO ATNM-DISPENSED-AMT
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 3.00 TO ATNM-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED = WS-WORK-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-DISPENSED-AMT > 0 AND ATNM-DISPENSED-AMT < 1.00
               MOVE 1.00 TO ATNM-DISPENSED-AMT
           END-IF
           IF ATNM-DISPENSED-AMT > 50.00
               MOVE 50.00 TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DISPENSED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REGION-CD
                   ADD ATNM-DISPENSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-DISPENSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTERCHG AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4090-LIMIT-INTERCHG.
           IF ATNM-INTERCHG-AMT < 0.00
               MOVE 0.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-INTERCHG-AMT > 500.00
               MOVE 500.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE DISPENSED YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4100-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-FEE-PCT / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 10.00 TO ATNM-DISPENSED-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 15.00 TO ATNM-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                       ATNM-DISPENSED-YTD * ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-DISPENSED-AMT > 0 AND ATNM-DISPENSED-AMT < 2.00
               MOVE 2.00 TO ATNM-DISPENSED-AMT
           END-IF
           IF ATNM-DISPENSED-AMT > 35.00
               MOVE 35.00 TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ATNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE INTERCHG AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4130-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 1.00
               MOVE 1.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 150.00
               MOVE 150.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-MATURITY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4150-FLAG-SETTLE.
           IF ATNM-SETTLE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4160-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURCHG AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4170-LIMIT-SURCHG.
           IF ATNT-SURCHG-AMT < 25.00
               MOVE 25.00 TO ATNT-SURCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNT-SURCHG-AMT > 500.00
               MOVE 500.00 TO ATNT-SURCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST FEE ON THE SETTLE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4180-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-RATE / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 3.00 TO ATNM-ADJUST-AMT
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 12.00 TO ATNM-ADJUST-AMT
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-ADJUST-AMT > 0 AND ATNM-ADJUST-AMT < 5.00
               MOVE 5.00 TO ATNM-ADJUST-AMT
           END-IF
           IF ATNM-ADJUST-AMT > 35.00
               MOVE 35.00 TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASSETTE YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4190-LIMIT-CASSETTE.
           IF ATNM-CASSETTE-YTD < 100.00
               MOVE 100.00 TO ATNM-CASSETTE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-CASSETTE-YTD > 5000.00
               MOVE 5000.00 TO ATNM-CASSETTE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST AMT BY NETWORK CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-NETWORK.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-NETWORK-CD
                   ADD ATNM-ADJUST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-NETWORK-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-ADJUST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E411 CUST ID REQUIRED
      * E226 CASSETTE AMT NEGATIVE
      * E069 CASSETTE AMT OVER 50000
      * E697 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CUST-ID = SPACES
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN ATNT-CASSETTE-AMT < 0
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN ATNT-CASSETTE-AMT > 50000.00
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN ATNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-NEXT-DUE.
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
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTERCHG YTD.
      *----------------------------------------------------------------*
       4230-PRORATE-INTERCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-INTERCHG-YTD ROUNDED =
                   ATNM-SETTLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-INTERCHG-YTD
           END-IF
           ADD ATNM-INTERCHG-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE SURCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNM-SURCHG-AMT NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN NEXT REVIEW DATE IS PAST
      * G -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> P WHEN NEXT REVIEW DATE IS PAST
      * P -> V WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4250-SET-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'G' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4260-SPLIT-CASSETTE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ATNM-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURCHG AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4270-SPLIT-SURCHG.
           COMPUTE WS-WORK-AMT = ATNM-SURCHG-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SURCHG-AMT - (WS-WORK-AMT * 3)
           COMPUTE ATNM-ADJUST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4280-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 5
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-EFF.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE CASSETTE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4310-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED = ATNT-CASSETTE-AMT
                       * ATNM-FEE-PCT / 200
               WHEN ATNM-REGION-CD = 'F'
                   MOVE 3.00 TO ATNM-CASSETTE-AMT
               WHEN ATNM-REGION-CD = 'G'
                   MOVE 12.00 TO ATNM-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED = ATNT-CASSETTE-AMT
                       * ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-CASSETTE-AMT > 0 AND ATNM-CASSETTE-AMT < 2.00
               MOVE 2.00 TO ATNM-CASSETTE-AMT
           END-IF
           IF ATNM-CASSETTE-AMT > 250.00
               MOVE 250.00 TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ADJUST AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4320-LIMIT-ADJUST.
           IF ATNM-ADJUST-AMT < 0.00
               MOVE 0.00 TO ATNM-ADJUST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-ADJUST-AMT > 500.00
               MOVE 500.00 TO ATNM-ADJUST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURCHG AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4330-FLAG-SURCHG.
           IF ATNT-SURCHG-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNT-SURCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4340-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE CASSETTE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNT-CASSETTE-AMT NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE SURCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4360-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ATNM-SURCHG-AMT NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASSETTE YTD.
      *----------------------------------------------------------------*
       4370-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-YTD ROUNDED =
                   ATNT-SURCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4380-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4390-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4400-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTERCHG YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = ATNM-INTERCHG-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-INTERCHG-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ATNM-SURCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TERMINAL CD:
      * BJ -> JJ WHEN LAST PAY DATE IS PAST
      * JJ -> JZ WHEN LAST PAY DATE IS PAST
      * JZ -> KK WHEN LAST PAY DATE IS PAST
      * KK -> BJ WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4420-SET-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JJ'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KK' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KK'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO ATNM-TERMINAL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4430-ACCRUE-SETTLE.
           IF ATNM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SETTLE-YTD * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-CHANNEL-CD
                   ADD ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4450-FLAG-SETTLE.
           IF ATNM-SETTLE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       4460-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-YTD ROUNDED =
                   ATNT-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4470-AGE-POST.
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
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E773 MEMO REQUIRED
      * E072 WORK AMT NEGATIVE
      * E158 WORK AMT OVER 1000000
      * E603 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4480-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-MEMO = SPACES
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E072' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E158' TO WS-REJECT-CODE
               WHEN ATNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E295 ADDR REQUIRED
      * E601 CASSETTE AMT NEGATIVE
      * E070 CASSETTE AMT OVER 250000
      * E504 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4490-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-AMT < 0
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-AMT > 250000.00
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN ATNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4500-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BANDX TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BANDX-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-INTERCHG-YTD NOT < WS-SETTLE-BANDX-LOW(WS-TX)
                   MOVE WS-SETTLE-BANDX-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BANDX TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4520-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BANDX-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNT-DISPENSED-AMT NOT < WS-SETTLE-BANDX-LOW(WS-TX)
                   MOVE WS-SETTLE-BANDX-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4530-AGE-OPEN.
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
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4540-ACCRUE-SURCHG.
           IF ATNM-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SURCHG-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-CASSETTE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4550-FLAG-DISPENSED.
           IF ATNM-DISPENSED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DU -> HX WHEN POST DATE IS PAST
      * HX -> YC WHEN POST DATE IS PAST
      * YC -> ZB WHEN POST DATE IS PAST
      * ZB -> DU WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4560-SET-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZB' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZB'
                   IF ATNT-POST-DATE > 0 AND ATNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO ATNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISPENSED AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4570-LIMIT-DISPENSED.
           IF ATNM-DISPENSED-AMT < 50.00
               MOVE 50.00 TO ATNM-DISPENSED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-DISPENSED-AMT > 10000.00
               MOVE 10000.00 TO ATNM-DISPENSED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4580-DESCRIBE-BRANCH.
           EVALUATE ATNM-BRANCH-CD
               WHEN 'DU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4590-SPLIT-INTERCHG.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE ATNM-INTERCHG-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4600-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4610-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNM-SETTLE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ATNM-SETTLE-YTD - (WS-WORK-AMT * 2)
           COMPUTE ATNM-INTERCHG-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4620-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN EFFECTIVE DATE IS PAST
      * HX -> PR WHEN EFFECTIVE DATE IS PAST
      * PR -> XD WHEN EFFECTIVE DATE IS PAST
      * XD -> ZJ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4630-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADJUST YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4640-FLAG-ADJUST.
           IF ATNM-ADJUST-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-ADJUST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST YTD FEE ON THE CASSETTE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4650-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-FEE-PCT / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 7.00 TO ATNM-ADJUST-YTD
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 15.00 TO ATNM-ADJUST-YTD
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-ADJUST-YTD > 0 AND ATNM-ADJUST-YTD < 10.00
               MOVE 10.00 TO ATNM-ADJUST-YTD
           END-IF
           IF ATNM-ADJUST-YTD > 50.00
               MOVE 50.00 TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4660-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNM-SETTLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SETTLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4670-CHECK-NOTICE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4680-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 10
               MOVE 'Y' TO ATNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4690-AGE-EXPIRY.
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
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN NEXT REVIEW DATE IS PAST
      * HX -> PR WHEN NEXT REVIEW DATE IS PAST
      * PR -> XD WHEN NEXT REVIEW DATE IS PAST
      * XD -> ZJ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4700-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E262 ADDR REQUIRED
      * E169 SURCHG AMT NEGATIVE
      * E598 SURCHG AMT OVER 25000
      * E515 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4710-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-ADDR = SPACES
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT < 0
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT > 25000.00
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4720-FLAG-CASSETTE.
           IF ATNT-CASSETTE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNT-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4730-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASSETTE AMT.
      *----------------------------------------------------------------*
       4740-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-AMT ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISPENSED AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4750-LIMIT-DISPENSED.
           IF ATNM-DISPENSED-AMT < 10.00
               MOVE 10.00 TO ATNM-DISPENSED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-DISPENSED-AMT > 5000.00
               MOVE 5000.00 TO ATNM-DISPENSED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4760-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       ATNM-DISCOUNT / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 7.00 TO ATNM-SETTLE-AMT
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 10.00 TO ATNM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-SETTLE-AMT > 0 AND ATNM-SETTLE-AMT < 10.00
               MOVE 10.00 TO ATNM-SETTLE-AMT
           END-IF
           IF ATNM-SETTLE-AMT > 50.00
               MOVE 50.00 TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASSETTE AMT.
      *----------------------------------------------------------------*
       4770-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-AMT ROUNDED =
                   ATNT-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED YTD FEE ON THE SURCHG YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4780-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-DISPENSED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-DISPENSED-YTD ROUNDED = ATNM-SURCHG-YTD
                       * ATNM-RATE / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 10.00 TO ATNM-DISPENSED-YTD
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 7.00 TO ATNM-DISPENSED-YTD
               WHEN OTHER
                   COMPUTE ATNM-DISPENSED-YTD ROUNDED = ATNM-SURCHG-YTD
                       * ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-DISPENSED-YTD > 0 AND ATNM-DISPENSED-YTD < 5.00
               MOVE 5.00 TO ATNM-DISPENSED-YTD
           END-IF
           IF ATNM-DISPENSED-YTD > 50.00
               MOVE 50.00 TO ATNM-DISPENSED-YTD
           END-IF
           ADD ATNM-DISPENSED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4790-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 3
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ATN FIELDS TO ATWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-ATWAIV.
           INITIALIZE ATWAIV-PARMS
           MOVE ATNM-INTERCHG-YTD
             TO ATWAIV-INTERCHG-YTD
           MOVE ATNM-SETTLE-AMT
             TO ATWAIV-SETTLE-AMT
           MOVE ATNM-ADJUST-YTD
             TO ATWAIV-ADJUST-YTD
           MOVE ATNM-DISPENSED-YTD
             TO ATWAIV-DISPENSED-YTD
           MOVE ATNM-ADJUST-AMT
             TO ATWAIV-ADJUST-AMT
           MOVE ATNM-SETTLE-YTD
             TO ATWAIV-SETTLE-YTD
           MOVE ATNM-NEXT-REVIEW-DATE
             TO ATWAIV-NEXT-REVIEW-DATE
           MOVE ATNM-MATURITY-DATE
             TO ATWAIV-MATURITY-DATE
           MOVE ATNM-LAST-PAY-DATE
             TO ATWAIV-LAST-PAY-DATE
           MOVE ATNM-EFFECTIVE-DATE
             TO ATWAIV-EFFECTIVE-DATE
           MOVE ATNM-BRANCH-CD
             TO ATWAIV-BRANCH-CD
           MOVE ATNM-TERMINAL-CD
             TO ATWAIV-TERMINAL-CD
           MOVE ATNM-NETWORK-CD
             TO ATWAIV-NETWORK-CD
           MOVE ATNM-STATUS-CD
             TO ATWAIV-STATUS-CD
           MOVE ATNM-SUSPEND-FLAG
             TO ATWAIV-SUSPEND-FLAG
           MOVE ATNM-NOTICE-SENT-FLAG
             TO ATWAIV-NOTICE-SENT-FLAG
           MOVE ATNM-REVIEW-FLAG
             TO ATWAIV-REVIEW-FLAG
           MOVE ATNM-PAYMENT-CNT
             TO ATWAIV-PAYMENT-CNT
           MOVE ATNM-ITEM-CNT
             TO ATWAIV-ITEM-CNT
           MOVE ATNM-FEE-PCT
             TO ATWAIV-FEE-PCT
           MOVE ATNM-RATE
             TO ATWAIV-RATE
           MOVE ATNM-DESC
             TO ATWAIV-DESC
           MOVE ATNM-NAME
             TO ATWAIV-NAME
           CALL 'ATWAIV' USING ATWAIV-PARMS
           EVALUATE ATWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ATWAIV-INTERCHG-YTD
                     TO ATNM-INTERCHG-YTD
                   MOVE ATWAIV-ADJUST-YTD
                     TO ATNM-ADJUST-YTD
                   MOVE ATWAIV-DISPENSED-YTD
                     TO ATNM-DISPENSED-YTD
                   MOVE ATWAIV-ADJUST-AMT
                     TO ATNM-ADJUST-AMT
                   MOVE ATWAIV-SETTLE-YTD
                     TO ATNM-SETTLE-YTD
                   MOVE ATWAIV-SUSPEND-FLAG
                     TO ATNM-SUSPEND-FLAG
                   MOVE ATWAIV-NOTICE-SENT-FLAG
                     TO ATNM-NOTICE-SENT-FLAG
                   MOVE ATWAIV-REVIEW-FLAG
                     TO ATNM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-ATWAIV' TO ABND-PARAGRAPH
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
           DISPLAY 'ATNSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ATNSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
