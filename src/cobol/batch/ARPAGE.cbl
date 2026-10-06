      *================================================================*
      * PROGRAM  : ARPAGE                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ACCOUNT RECONCILEMENT - AGING. READS THE ARP        *
      *            TRANSACTIONS, UPDATES THE ARP MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FULL AND PARTIAL ACCOUNT        *
      *            RECONCILEMENT SERVICE FOR CORPORATES                *
      * INPUT    : ARPMST (KSDS), ARPTRN, BANKCTL                      *
      * OUTPUT   : ARPOUT, ARPAGRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ARPRC                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-28 RJT  ORIGINAL                                       *
      * 1995-04-18 PQR  VOID EXCEPTIONS TO OPS QUEUE INC1879           *
      * 1998-07-03 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-11-26 TWH  FEE FEE SCHEDULE UPDATE REQ2787                *
      * 2005-03-11 MKL  VOID CALCULATION CORRECTED PRB1812             *
      * 2006-05-27 EWS  ISSUED LIMIT REVISED CHG2848                   *
      * 2007-04-21 DPS  ISSUED TABLE EXPANDED CHG4481                  *
      * 2011-12-04 SKN  STOP EXCEPTIONS TO OPS QUEUE INC3864           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPAGE.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ARPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ARPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ARPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ARPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ARPAGRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ARPMST.
       FD  TRAN-FILE.
       COPY ARPTRN.
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
      * WS-PAID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000499000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000893500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000139000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000680500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000649000'.
       01  WS-PAID-TIER-TBL REDEFINES WS-PAID-TIER-VALUES.
           05  WS-PAID-TIER-ROW OCCURS 5 TIMES.
               10  WS-PAID-TIER-LOW        PIC 9(09)V99.
               10  WS-PAID-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000859500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000131500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000601000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000484000'.
       01  WS-VOID-GRID-TBL REDEFINES WS-VOID-GRID-VALUES.
           05  WS-VOID-GRID-ROW OCCURS 4 TIMES.
               10  WS-VOID-GRID-LOW        PIC 9(09)V99.
               10  WS-VOID-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000695000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000634000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000514500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000620000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003800000000873000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003850000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004100000000016000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004600000000216000'.
       01  WS-VOID-TIER-TBL REDEFINES WS-VOID-TIER-VALUES.
           05  WS-VOID-TIER-ROW OCCURS 8 TIMES.
               10  WS-VOID-TIER-LOW        PIC 9(09)V99.
               10  WS-VOID-TIER-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ARPAGE  ACCOUNT RECONCILEMENT'.
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
       COPY ARPRCP.

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
               MOVE 'ARPMST' TO ABND-FILE-NAME
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
           MOVE ARPT-KEY TO ARPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-CYCLE
               PERFORM 3010-SELECT-FEE-PCT
               PERFORM 3020-CHECK-LAST-ACT
               PERFORM 3030-CHECK-CLOSE
               PERFORM 3040-ACCUM-CHANNEL
               PERFORM 3050-ACCUM-BRANCH
               PERFORM 3060-LIMIT-PAID
               PERFORM 3070-EDIT-CUST-ID
               PERFORM 3080-ACCRUE-ISSUED
               PERFORM 3090-EDIT-REJECT
               PERFORM 3100-CHECK-LAST-PAY
               PERFORM 3110-FLAG-VOID
               PERFORM 3120-CHECK-LAST-ACT
               PERFORM 3130-AGE-NEXT-DUE
               PERFORM 3140-COUNT-ITEM
               PERFORM 3150-COUNT-PAYMENT
               PERFORM 3160-CHECK-EFF
               PERFORM 3170-LIMIT-OUTSTAND
               PERFORM 3180-DESCRIBE-CYCLE
               PERFORM 3190-DESCRIBE-SOURCE
               PERFORM 3200-ACCRUE-STOP
               PERFORM 3210-LIMIT-ISSUED
               PERFORM 3220-ASSESS-HOLD
               PERFORM 3230-LIMIT-OUTSTAND
               PERFORM 3240-CHECK-CLOSE
               PERFORM 3250-LIMIT-OUTSTAND
               PERFORM 3260-EDIT-DESC
               PERFORM 3270-PRORATE-FEE
               PERFORM 3280-ACCRUE-VOID
               PERFORM 3290-FLAG-VOID
               PERFORM 3300-FLAG-STOP
               PERFORM 3310-FLAG-ITEM
               PERFORM 3320-EDIT-CUST-ID
               PERFORM 3330-PRORATE-OUTSTAND
               PERFORM 3340-DESCRIBE-SOURCE
               PERFORM 3350-SPLIT-PAID
               PERFORM 3360-PRORATE-STOP
               PERFORM 3370-ACCUM-REGION
               PERFORM 3380-CHECK-EFF
               PERFORM 3390-SPLIT-STOP
               PERFORM 3400-PRORATE-PAID
               PERFORM 3410-EDIT-REF-NO
               PERFORM 3420-PRORATE-ISSUED
               PERFORM 3430-ACCUM-CYCLE
               PERFORM 3440-ACCRUE-VOID
               PERFORM 3450-SET-SOURCE
               PERFORM 3460-EDIT-MEMO
               PERFORM 3470-LIMIT-ISSUED
               PERFORM 3480-COUNT-EXCEPT
               PERFORM 3490-ACCUM-CHANNEL
               PERFORM 3500-SET-BRANCH
               PERFORM 3510-SPLIT-STOP
               PERFORM 3520-PRORATE-VOID
               PERFORM 3530-FLAG-ITEM
               PERFORM 3540-DESCRIBE-BRANCH
               PERFORM 3550-CHECK-NEXT-DUE
               PERFORM 3560-EDIT-MEMO
               PERFORM 3570-DESCRIBE-TRAN
               PERFORM 3580-SPLIT-VOID
               PERFORM 3590-SELECT-PENALTY-PCT
               PERFORM 3600-PRORATE-FEE
               PERFORM 3610-ACCRUE-STOP
               PERFORM 3620-SELECT-FEE-PCT
               PERFORM 3630-COUNT-ITEM
               PERFORM 3640-ACCUM-BRANCH
               PERFORM 3650-SELECT-PENALTY-PCT
               PERFORM 3660-SELECT-DISCOUNT
               PERFORM 3670-SELECT-DISCOUNT
               PERFORM 3680-ACCRUE-ISSUED
               PERFORM 3690-CHECK-OPEN
               PERFORM 3700-SELECT-PENALTY-PCT
               PERFORM 3710-PRORATE-FEE
               PERFORM 3720-ACCUM-REGION
               PERFORM 3730-ACCUM-TRAN
               PERFORM 3740-SELECT-PENALTY-PCT
               PERFORM 3750-COUNT-EXCEPT
               PERFORM 3760-ACCUM-CHANNEL
               PERFORM 3770-ACCRUE-ISSUED
               PERFORM 3780-LIMIT-FEE
               PERFORM 3790-EDIT-OPERATOR
               PERFORM 3800-ASSESS-EXEMPT
               PERFORM 3810-SPLIT-VOID
               PERFORM 3820-COUNT-PAYMENT
               PERFORM 3830-SET-CYCLE
               PERFORM 3840-SELECT-DISCOUNT
               PERFORM 3850-AGE-CLOSE
               PERFORM 3860-SELECT-PENALTY-PCT
               PERFORM 3870-SET-CHANNEL
               PERFORM 3880-ACCUM-TRAN
               PERFORM 3890-DESCRIBE-RECON-TYPE
               PERFORM 3900-DESCRIBE-CHANNEL
               PERFORM 3910-COUNT-EXCEPT
               PERFORM 3920-FLAG-FEE
               PERFORM 3930-EDIT-NAME
               PERFORM 3940-CHECK-NEXT-REVIEW
               PERFORM 3950-EDIT-REJECT
               PERFORM 3960-SPLIT-ISSUED
               PERFORM 3970-EDIT-OPERATOR
               PERFORM 3980-CHECK-POST
               PERFORM 3990-SELECT-DISCOUNT
               PERFORM 4000-FLAG-VOID
               PERFORM 4010-DESCRIBE-TRAN
               PERFORM 4020-ASSESS-PREFERRED
               PERFORM 4030-SELECT-DISCOUNT
               PERFORM 4040-ACCUM-RECON-TYPE
               PERFORM 4050-COUNT-CYCLE
               PERFORM 4060-ASSESS-EXEMPT
               PERFORM 4070-LIMIT-STOP
               PERFORM 4080-COUNT-ITEM
               PERFORM 4090-DESCRIBE-RECON-TYPE
               PERFORM 4100-SPLIT-FEE
               PERFORM 4110-CHECK-NEXT-REVIEW
               PERFORM 4120-SPLIT-STOP
               PERFORM 4130-PRORATE-ISSUED
               PERFORM 4140-SELECT-DISCOUNT
               PERFORM 4150-PRORATE-VOID
               PERFORM 4160-CHECK-NEXT-REVIEW
               PERFORM 4170-ACCRUE-FEE
               PERFORM 4180-FLAG-ISSUED
               PERFORM 4190-ACCUM-CHANNEL
               PERFORM 4200-SELECT-DISCOUNT
               PERFORM 4210-DESCRIBE-BRANCH
               PERFORM 4220-PRORATE-VOID
               PERFORM 4230-ACCUM-CHANNEL
               PERFORM 4240-CHECK-LAST-ACT
               PERFORM 4250-SELECT-FEE-PCT
               PERFORM 4260-SELECT-PENALTY-PCT
               PERFORM 4270-ACCRUE-VOID
               PERFORM 4280-PRORATE-PAID
               PERFORM 4290-SET-CYCLE
               PERFORM 4300-PRORATE-PAID
               PERFORM 4310-SPLIT-ISSUED
               PERFORM 4320-PRORATE-PAID
               PERFORM 4330-ACCUM-RECON-TYPE
               PERFORM 4340-ACCUM-CHANNEL
               PERFORM 4350-CHECK-EFFECTIVE
               PERFORM 4360-EDIT-REJECT
               PERFORM 4370-LIMIT-FEE
               PERFORM 4380-EDIT-REJECT
               PERFORM 4390-CHECK-POST
               PERFORM 4400-PRORATE-FEE
               PERFORM 4410-SET-BRANCH
               PERFORM 4420-ASSESS-PREFERRED
               PERFORM 4430-SPLIT-VOID
               PERFORM 4440-ASSESS-EXEMPT
               PERFORM 4450-DESCRIBE-CHANNEL
               PERFORM 4460-SET-BRANCH
               PERFORM 4470-SELECT-DISCOUNT
               PERFORM 4480-PRORATE-FEE
               PERFORM 4490-ACCRUE-PENALTY-PCT
               PERFORM 4500-PRORATE-VOID
               PERFORM 4510-SPLIT-FEE
               PERFORM 7000-CALL-ARPRC
               REWRITE ARP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ARPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ARP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CYCLE-CD
                   ADD ARPM-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID GRID TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-GRID-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ARPT-ISSUED-AMT NOT < WS-VOID-GRID-LOW(WS-TX)
                   MOVE WS-VOID-GRID-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD ARPM-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-BRANCH-CD
                   ADD ARPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3060-LIMIT-PAID.
           IF ARPM-PAID-YTD < 0.00
               MOVE 0.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-PAID-YTD > 500.00
               MOVE 500.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E590 CUST ID REQUIRED
      * E186 VOID AMT NEGATIVE
      * E197 VOID AMT OVER 50000
      * E359 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN ARPT-VOID-AMT < 0
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN ARPT-VOID-AMT > 50000.00
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-ISSUED.
           IF ARPM-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-YTD * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E362 REJECT CD REQUIRED
      * E380 VOID AMT NEGATIVE
      * E849 VOID AMT OVER 10000
      * E475 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN ARPM-VOID-AMT < 0
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN ARPM-VOID-AMT > 10000.00
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3110-FLAG-VOID.
           IF ARPM-VOID-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ARPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 6
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ARPT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3170-LIMIT-OUTSTAND.
           IF ARPM-OUTSTAND-AMT < 0.00
               MOVE 0.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-OUTSTAND-AMT > 500.00
               MOVE 500.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-STOP.
           IF ARPM-STOP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-AMT * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPT-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3210-LIMIT-ISSUED.
           IF ARPM-ISSUED-AMT < 25.00
               MOVE 25.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-AMT > 1000.00
               MOVE 1000.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE STOP AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3220-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPM-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-ISSUED-YTD ROUNDED = ARPM-STOP-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 12.00 TO ARPM-ISSUED-YTD
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 3.00 TO ARPM-ISSUED-YTD
               WHEN OTHER
                   COMPUTE ARPM-ISSUED-YTD ROUNDED = ARPM-STOP-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-ISSUED-YTD > 0 AND ARPM-ISSUED-YTD < 1.00
               MOVE 1.00 TO ARPM-ISSUED-YTD
           END-IF
           IF ARPM-ISSUED-YTD > 25.00
               MOVE 25.00 TO ARPM-ISSUED-YTD
           END-IF
           ADD ARPM-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3230-LIMIT-OUTSTAND.
           IF ARPT-OUTSTAND-AMT < 100.00
               MOVE 100.00 TO ARPT-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-OUTSTAND-AMT > 500.00
               MOVE 500.00 TO ARPT-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3250-LIMIT-OUTSTAND.
           IF ARPM-OUTSTAND-YTD < 100.00
               MOVE 100.00 TO ARPM-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-OUTSTAND-YTD > 500.00
               MOVE 500.00 TO ARPM-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E644 DESC REQUIRED
      * E001 FEE AMT NEGATIVE
      * E662 FEE AMT OVER 10000
      * E693 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT < 0
                   MOVE 'E001' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT > 10000.00
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN ARPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-AMT ROUNDED =
                   ARPM-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-VOID.
           IF ARPM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-YTD * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3290-FLAG-VOID.
           IF ARPM-VOID-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STOP AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3300-FLAG-STOP.
           IF ARPM-STOP-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-STOP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E733 CUST ID REQUIRED
      * E226 PAID AMT NEGATIVE
      * E146 PAID AMT OVER 250000
      * E289 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E733' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT < 0
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT > 250000.00
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3330-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   ARPM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARPM-PAID-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-PAID-YTD - (WS-WORK-AMT * 3)
           COMPUTE ARPM-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STOP YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-YTD ROUNDED =
                   ARPM-OUTSTAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-REGION-CD
                   ADD ARPM-PAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-PAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ARPT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STOP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARPM-STOP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-STOP-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-AMT ROUNDED =
                   ARPM-OUTSTAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E404 REF NO REQUIRED
      * E224 ISSUED AMT NEGATIVE
      * E432 ISSUED AMT OVER 99999
      * E787 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-REF-NO = SPACES
                   MOVE 'E404' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-AMT < 0
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-AMT > 99999.00
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN ARPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3420-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-ISSUED-AMT ROUNDED =
                   ARPM-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-ISSUED-AMT
           END-IF
           ADD ARPT-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CYCLE-CD
                   ADD ARPM-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-VOID.
           IF ARPM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-YTD * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN EFFECTIVE DATE IS PAST
      * MB -> MH WHEN EFFECTIVE DATE IS PAST
      * MH -> UG WHEN EFFECTIVE DATE IS PAST
      * UG -> UR WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E270 MEMO REQUIRED
      * E672 OUTSTAND AMT NEGATIVE
      * E615 OUTSTAND AMT OVER 5000
      * E596 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-MEMO = SPACES
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT < 0
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT > 5000.00
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3470-LIMIT-ISSUED.
           IF ARPM-ISSUED-AMT < 0.00
               MOVE 0.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-AMT > 50000.00
               MOVE 50000.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ARPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-CHANNEL-CD
                   ADD ARPM-PAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-PAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN NEXT DUE DATE IS PAST
      * GM -> GV WHEN NEXT DUE DATE IS PAST
      * GV -> HK WHEN NEXT DUE DATE IS PAST
      * HK -> VX WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STOP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARPM-STOP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-STOP-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARPM-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       3520-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-VOID-YTD ROUNDED =
                   ARPM-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3530-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E253 MEMO REQUIRED
      * E122 STOP AMT NEGATIVE
      * E678 STOP AMT OVER 99999
      * E291 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3560-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-MEMO = SPACES
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN ARPM-STOP-AMT < 0
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN ARPM-STOP-AMT > 99999.00
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN ARPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E291' TO WS-REJECT-CODE
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
       3570-DESCRIBE-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPM-VOID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-VOID-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPT-VOID-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3600-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-YTD ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-STOP.
           IF ARPM-STOP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-YTD * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPM-VOID-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-BRANCH-CD
                   ADD ARPM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-GRID-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ARPM-FEE-YTD NOT < WS-VOID-GRID-LOW(WS-TX)
                   MOVE WS-VOID-GRID-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPM-FEE-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-ISSUED.
           IF ARPM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3690-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID GRID TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3700-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-GRID-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ARPM-ISSUED-YTD NOT < WS-VOID-GRID-LOW(WS-TX)
                   MOVE WS-VOID-GRID-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-FEE-AMT ROUNDED =
                   ARPM-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-FEE-AMT
           END-IF
           ADD ARPT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE STOP YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-REGION-CD
                   ADD ARPM-STOP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-STOP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD ARPM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-ISSUED-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD ARPT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-ISSUED.
           IF ARPM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-AMT * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-FEE.
           IF ARPM-FEE-YTD < 25.00
               MOVE 25.00 TO ARPM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-FEE-YTD > 50000.00
               MOVE 50000.00 TO ARPM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E674 OPERATOR REQUIRED
      * E087 STOP YTD NEGATIVE
      * E378 STOP YTD OVER 50000
      * E317 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-OPERATOR = SPACES
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN ARPM-STOP-YTD < 0
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN ARPM-STOP-YTD > 50000.00
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE STOP AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3800-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE ARPM-OUTSTAND-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 15.00 TO ARPM-OUTSTAND-AMT
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 5.00 TO ARPM-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE ARPM-OUTSTAND-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-OUTSTAND-AMT > 0 AND ARPM-OUTSTAND-AMT < 1.00
               MOVE 1.00 TO ARPM-OUTSTAND-AMT
           END-IF
           IF ARPM-OUTSTAND-AMT > 100.00
               MOVE 100.00 TO ARPM-OUTSTAND-AMT
           END-IF
           ADD ARPM-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPT-VOID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPT-VOID-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CP -> MR WHEN LAST PAY DATE IS PAST
      * MR -> PY WHEN LAST PAY DATE IS PAST
      * PY -> SP WHEN LAST PAY DATE IS PAST
      * SP -> TP WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPT-OUTSTAND-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPM-ISSUED-YTD NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN LAST STMT DATE IS PAST
      * SW -> UA WHEN LAST STMT DATE IS PAST
      * UA -> WB WHEN LAST STMT DATE IS PAST
      * WB -> ME WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3920-FLAG-FEE.
           IF ARPT-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E896 NAME REQUIRED
      * E910 ISSUED YTD NEGATIVE
      * E852 ISSUED YTD OVER 250000
      * E973 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-NAME = SPACES
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD < 0
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD > 250000.00
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN ARPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E959 REJECT CD REQUIRED
      * E903 STOP YTD NEGATIVE
      * E003 STOP YTD OVER 99999
      * E386 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN ARPM-STOP-YTD < 0
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN ARPM-STOP-YTD > 99999.00
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-YTD - (WS-WORK-AMT * 12)
           COMPUTE ARPT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E003 OPERATOR REQUIRED
      * E088 OUTSTAND YTD NEGATIVE
      * E833 OUTSTAND YTD OVER 25000
      * E839 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-OPERATOR = SPACES
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E088' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 25000.00
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN ARPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ARPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-GRID-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-VOID-GRID-LOW(WS-TX)
                   MOVE WS-VOID-GRID-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4000-FLAG-VOID.
           IF ARPM-VOID-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STOP YTD FEE ON THE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4020-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-STOP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-STOP-YTD ROUNDED = ARPM-FEE-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO ARPM-STOP-YTD
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO ARPM-STOP-YTD
               WHEN OTHER
                   COMPUTE ARPM-STOP-YTD ROUNDED = ARPM-FEE-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-STOP-YTD > 0 AND ARPM-STOP-YTD < 2.00
               MOVE 2.00 TO ARPM-STOP-YTD
           END-IF
           IF ARPM-STOP-YTD > 50.00
               MOVE 50.00 TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-FEE-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-RECON-TYPE-CD
                   ADD ARPT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPT-FEE-AMT ROUNDED = ARPM-FEE-YTD *
                       ARPM-FEE-PCT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO ARPT-FEE-AMT
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO ARPT-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPT-FEE-AMT ROUNDED = ARPM-FEE-YTD *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPT-FEE-AMT > 0 AND ARPT-FEE-AMT < 10.00
               MOVE 10.00 TO ARPT-FEE-AMT
           END-IF
           IF ARPT-FEE-AMT > 150.00
               MOVE 150.00 TO ARPT-FEE-AMT
           END-IF
           ADD ARPT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4070-LIMIT-STOP.
           IF ARPM-STOP-AMT < 25.00
               MOVE 25.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 50000.00
               MOVE 50000.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STOP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARPM-STOP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-STOP-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARPM-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED YTD.
      *----------------------------------------------------------------*
       4130-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-ISSUED-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-ISSUED-YTD
           END-IF
           ADD ARPM-ISSUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPT-FEE-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STOP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-VOID-AMT ROUNDED =
                   ARPM-STOP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-VOID-AMT
           END-IF
           ADD ARPM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-FEE.
           IF ARPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-FEE-AMT * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPT-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4180-FLAG-ISSUED.
           IF ARPM-ISSUED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-VOID-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       4220-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-VOID-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4230-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-CHANNEL-CD
                   ADD ARPM-PAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-PAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-VOID-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-PAID-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-VOID.
           IF ARPM-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-AMT * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       4280-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-AMT ROUNDED =
                   ARPT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CP -> MR WHEN EFFECTIVE DATE IS PAST
      * MR -> PY WHEN EFFECTIVE DATE IS PAST
      * PY -> SP WHEN EFFECTIVE DATE IS PAST
      * SP -> TP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4300-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-YTD - (WS-WORK-AMT * 2)
           COMPUTE ARPT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       4320-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-AMT ROUNDED =
                   ARPT-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4330-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-RECON-TYPE-CD
                   ADD ARPM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STOP YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD ARPM-STOP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-STOP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E234 REJECT CD REQUIRED
      * E974 OUTSTAND YTD NEGATIVE
      * E133 OUTSTAND YTD OVER 1000000
      * E081 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4360-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E974' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 1000000.00
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4370-LIMIT-FEE.
           IF ARPT-FEE-AMT < 0.00
               MOVE 0.00 TO ARPT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-FEE-AMT > 10000.00
               MOVE 10000.00 TO ARPT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E430 REJECT CD REQUIRED
      * E886 PAID YTD NEGATIVE
      * E471 PAID YTD OVER 99999
      * E377 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4380-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN ARPM-PAID-YTD < 0
                   MOVE 'E886' TO WS-REJECT-CODE
               WHEN ARPM-PAID-YTD > 99999.00
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN ARPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ARPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STOP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4400-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-YTD ROUNDED =
                   ARPM-STOP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN LAST ACT DATE IS PAST
      * GM -> GV WHEN LAST ACT DATE IS PAST
      * GV -> HK WHEN LAST ACT DATE IS PAST
      * HK -> VX WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4410-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VOID YTD FEE ON THE ISSUED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4420-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ARPM-VOID-YTD ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 12.00 TO ARPM-VOID-YTD
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 7.00 TO ARPM-VOID-YTD
               WHEN OTHER
                   COMPUTE ARPM-VOID-YTD ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-VOID-YTD > 0 AND ARPM-VOID-YTD < 10.00
               MOVE 10.00 TO ARPM-VOID-YTD
           END-IF
           IF ARPM-VOID-YTD > 35.00
               MOVE 35.00 TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPT-VOID-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPT-VOID-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE PAID YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4440-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-ISSUED-AMT ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 3.00 TO ARPM-ISSUED-AMT
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 10.00 TO ARPM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPM-ISSUED-AMT ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-ISSUED-AMT > 0 AND ARPM-ISSUED-AMT < 10.00
               MOVE 10.00 TO ARPM-ISSUED-AMT
           END-IF
           IF ARPM-ISSUED-AMT > 50.00
               MOVE 50.00 TO ARPM-ISSUED-AMT
           END-IF
           ADD ARPM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4450-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN POST DATE IS PAST
      * GM -> GV WHEN POST DATE IS PAST
      * GV -> HK WHEN POST DATE IS PAST
      * HK -> VX WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4460-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE OUTSTAND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4470-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-OUTSTAND-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4480-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4490-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ARPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-STOP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4500-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4510-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-ARPRC.
           INITIALIZE ARPRC-PARMS
           MOVE ARPM-OUTSTAND-YTD
             TO ARPRC-OUTSTAND-YTD
           MOVE ARPM-ISSUED-AMT
             TO ARPRC-ISSUED-AMT
           MOVE ARPM-ISSUED-YTD
             TO ARPRC-ISSUED-YTD
           MOVE ARPM-FEE-YTD
             TO ARPRC-FEE-YTD
           MOVE ARPM-OUTSTAND-AMT
             TO ARPRC-OUTSTAND-AMT
           MOVE ARPM-STOP-YTD
             TO ARPRC-STOP-YTD
           MOVE ARPM-OPEN-DATE
             TO ARPRC-OPEN-DATE
           MOVE ARPM-NEXT-DUE-DATE
             TO ARPRC-NEXT-DUE-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARPRC-NEXT-REVIEW-DATE
           MOVE ARPM-LAST-ACT-DATE
             TO ARPRC-LAST-ACT-DATE
           MOVE ARPM-RECON-TYPE-CD
             TO ARPRC-RECON-TYPE-CD
           MOVE ARPM-BRANCH-CD
             TO ARPRC-BRANCH-CD
           MOVE ARPM-CHANNEL-CD
             TO ARPRC-CHANNEL-CD
           MOVE ARPM-SOURCE-CD
             TO ARPRC-SOURCE-CD
           MOVE ARPM-HOLD-FLAG
             TO ARPRC-HOLD-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARPRC-STOP-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARPRC-PREFERRED-FLAG
           MOVE ARPM-PAYMENT-CNT
             TO ARPRC-PAYMENT-CNT
           MOVE ARPM-EXCEPT-CNT
             TO ARPRC-EXCEPT-CNT
           MOVE ARPM-DISCOUNT
             TO ARPRC-DISCOUNT
           MOVE ARPM-PENALTY-PCT
             TO ARPRC-PENALTY-PCT
           MOVE ARPM-NAME
             TO ARPRC-NAME
           MOVE ARPM-ADDR
             TO ARPRC-ADDR
           CALL 'ARPRC' USING ARPRC-PARMS
           EVALUATE ARPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARPRC-ISSUED-AMT
                     TO ARPM-ISSUED-AMT
                   MOVE ARPRC-PREFERRED-FLAG
                     TO ARPM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ARPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ARPT-KEY TO RD-KEY
           MOVE ARPT-TRAN-CD TO RD-CODE
           MOVE ARPT-OUTSTAND-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ARPT-KEY TO RD-KEY
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
           DISPLAY 'ARPAGE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ARPAGE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
