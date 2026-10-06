      *================================================================*
      * PROGRAM  : PBKSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PASSBOOK PRINTING - STATEMENT PREPARATION. READS THE*
      *            PBK TRANSACTIONS, UPDATES THE PBK MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. PASSBOOK SAVINGS UPDATE*
      *            PRINTING AND UNPOSTED ITEM CONTROL                  *
      * INPUT    : PBKMST (KSDS), PBKTRN, BANKCTL                      *
      * OUTPUT   : PBKOUT, PBKSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PBSTAT, PBPRC           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-05-21 TWH  ORIGINAL                                       *
      * 1998-08-14 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-06-11 BHS  PRINTED STATUS CODES ADDED CHG6662             *
      * 2001-06-25 MKL  INTEREST FEE SCHEDULE UPDATE REQ2444           *
      * 2004-12-02 CRP  LINE CALCULATION CORRECTED INC0268             *
      * 2005-12-01 EWS  UNPOSTED CALCULATION CORRECTED INC8951         *
      * 2006-04-24 SKN  LINE ROUNDING - HALF UP INC7819                *
      * 2007-10-03 RJT  NEW PRINTED REPORT COLUMNS CHG4837             *
      * 2010-01-07 EWS  PRINTED NOTICE TEXT PER LEGAL CHG4465          *
      * 2011-05-20 RJT  CARRYFWD TABLE EXPANDED INC7705                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBKSTMT.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PBKMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PBKM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PBKTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PBKOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PBKSTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PBKMST.
       FD  TRAN-FILE.
       COPY PBKTRN.
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
      * WS-PRINTED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINTED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000428500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000896000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000638000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005650000000598000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006150000000086000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006650000000882000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011650000000133000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000498000'.
       01  WS-PRINTED-SCHED-TBL REDEFINES WS-PRINTED-SCHED-VALUES.
           05  WS-PRINTED-SCHED-ROW OCCURS 9 TIMES.
               10  WS-PRINTED-SCHED-LOW    PIC 9(09)V99.
               10  WS-PRINTED-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000281000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000468000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000679500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000421000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000567500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000487500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000691000'.
       01  WS-INTEREST-TIER-TBL REDEFINES WS-INTEREST-TIER-VALUES.
           05  WS-INTEREST-TIER-ROW OCCURS 7 TIMES.
               10  WS-INTEREST-TIER-LOW    PIC 9(09)V99.
               10  WS-INTEREST-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LINE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LINE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000201500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000849500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000860000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000853000'.
       01  WS-LINE-SCHED-TBL REDEFINES WS-LINE-SCHED-VALUES.
           05  WS-LINE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-LINE-SCHED-LOW       PIC 9(09)V99.
               10  WS-LINE-SCHED-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PBKSTMT  PASSBOOK PRINTING'.
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
       COPY PBSTATP.
       COPY PBPRCP.

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
               MOVE 'PBKMST' TO ABND-FILE-NAME
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
           MOVE PBKT-KEY TO PBKM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-CHANNEL
               PERFORM 3010-LIMIT-BALANCE
               PERFORM 3020-ACCRUE-PRINTED
               PERFORM 3030-FLAG-CARRYFWD
               PERFORM 3040-DESCRIBE-TRAN
               PERFORM 3050-SET-CHANNEL
               PERFORM 3060-CHECK-POST
               PERFORM 3070-AGE-NEXT-REVIEW
               PERFORM 3080-SET-CHANNEL
               PERFORM 3090-ACCRUE-PRINTED
               PERFORM 3100-LIMIT-BALANCE
               PERFORM 3110-CHECK-NEXT-DUE
               PERFORM 3120-ACCUM-CHANNEL
               PERFORM 3130-DESCRIBE-PRINTER
               PERFORM 3140-CHECK-MATURITY
               PERFORM 3150-EDIT-DESC
               PERFORM 3160-ASSESS-EXEMPT
               PERFORM 3170-ACCUM-CHANNEL
               PERFORM 3180-EDIT-DESC
               PERFORM 3190-ACCRUE-INTEREST
               PERFORM 3200-CHECK-CLOSE
               PERFORM 3210-EDIT-CUST-ID
               PERFORM 3220-ACCRUE-CARRYFWD
               PERFORM 3230-DESCRIBE-SOURCE
               PERFORM 3240-SELECT-MARGIN
               PERFORM 3250-PRORATE-BALANCE
               PERFORM 3260-FLAG-BALANCE
               PERFORM 3270-COUNT-PAYMENT
               PERFORM 3280-PRORATE-LINE
               PERFORM 3290-EDIT-OPERATOR
               PERFORM 3300-LIMIT-CARRYFWD
               PERFORM 3310-PRORATE-PRINTED
               PERFORM 3320-ACCUM-PRINTER
               PERFORM 3330-PRORATE-LINE
               PERFORM 3340-SET-BOOK-TYPE
               PERFORM 3350-CHECK-EFF
               PERFORM 3360-ACCRUE-UNPOSTED
               PERFORM 3370-SPLIT-PRINTED
               PERFORM 3380-PRORATE-BALANCE
               PERFORM 3390-CHECK-MATURITY
               PERFORM 3400-SET-CHANNEL
               PERFORM 3410-ASSESS-NOTICE-SENT
               PERFORM 3420-COUNT-PAYMENT
               PERFORM 3430-ACCRUE-CARRYFWD
               PERFORM 3440-FLAG-PRINTED
               PERFORM 3450-SPLIT-LINE
               PERFORM 3460-AGE-NEXT-DUE
               PERFORM 3470-ACCUM-PRINTER
               PERFORM 3480-SPLIT-CARRYFWD
               PERFORM 3490-FLAG-CARRYFWD
               PERFORM 3500-ACCRUE-PRINTED
               PERFORM 3510-DESCRIBE-TRAN
               PERFORM 3520-EDIT-OPERATOR
               PERFORM 3530-ASSESS-NOTICE-SENT
               PERFORM 3540-DESCRIBE-SOURCE
               PERFORM 3550-EDIT-DESC
               PERFORM 3560-SET-PRINTER
               PERFORM 3570-ACCRUE-BALANCE
               PERFORM 3580-FLAG-INTEREST
               PERFORM 3590-ASSESS-EXEMPT
               PERFORM 3600-CHECK-NEXT-DUE
               PERFORM 3610-FLAG-INTEREST
               PERFORM 3620-AGE-EFF
               PERFORM 3630-ACCUM-PRINTER
               PERFORM 3640-EDIT-DESC
               PERFORM 3650-COUNT-PAYMENT
               PERFORM 3660-ACCUM-BOOK-TYPE
               PERFORM 3670-CHECK-MATURITY
               PERFORM 3680-LIMIT-CARRYFWD
               PERFORM 3690-ACCUM-TRAN
               PERFORM 3700-PRORATE-CARRYFWD
               PERFORM 3710-PRORATE-BALANCE
               PERFORM 3720-PRORATE-INTEREST
               PERFORM 3730-ASSESS-EXEMPT
               PERFORM 3740-DESCRIBE-CHANNEL
               PERFORM 3750-PRORATE-LINE
               PERFORM 3760-ASSESS-AUTO-RENEW
               PERFORM 3770-AGE-OPEN
               PERFORM 3780-ACCRUE-UNPOSTED
               PERFORM 3790-SPLIT-INTEREST
               PERFORM 3800-AGE-NEXT-DUE
               PERFORM 3810-SPLIT-PRINTED
               PERFORM 3820-EDIT-CONTACT
               PERFORM 3830-PRORATE-PRINTED
               PERFORM 3840-CHECK-LAST-ACT
               PERFORM 3850-FLAG-INTEREST
               PERFORM 3860-SELECT-FEE-PCT
               PERFORM 3870-ACCUM-REGION
               PERFORM 3880-SET-BRANCH
               PERFORM 3890-SPLIT-PRINTED
               PERFORM 3900-ACCUM-CHANNEL
               PERFORM 3910-DESCRIBE-BRANCH
               PERFORM 3920-EDIT-ADDR
               PERFORM 3930-LIMIT-BALANCE
               PERFORM 3940-SELECT-PENALTY-PCT
               PERFORM 3950-AGE-OPEN
               PERFORM 3960-ACCRUE-PRINTED
               PERFORM 3970-SELECT-MARGIN
               PERFORM 3980-LIMIT-CARRYFWD
               PERFORM 3990-CHECK-OPEN
               PERFORM 4000-AGE-NEXT-REVIEW
               PERFORM 4010-SET-TRAN
               PERFORM 4020-FLAG-INTEREST
               PERFORM 4030-AGE-EFFECTIVE
               PERFORM 4040-ACCUM-BRANCH
               PERFORM 4050-SET-REGION
               PERFORM 4060-ACCUM-TRAN
               PERFORM 4070-ACCRUE-CARRYFWD
               PERFORM 4080-SPLIT-BALANCE
               PERFORM 4090-LIMIT-CARRYFWD
               PERFORM 4100-COUNT-SEQ
               PERFORM 4110-PRORATE-INTEREST
               PERFORM 4120-SELECT-MARGIN
               PERFORM 4130-SELECT-MARGIN
               PERFORM 4140-SPLIT-CARRYFWD
               PERFORM 4150-SELECT-MARGIN
               PERFORM 4160-SPLIT-LINE
               PERFORM 4170-FLAG-UNPOSTED
               PERFORM 4180-PRORATE-CARRYFWD
               PERFORM 4190-ASSESS-EXEMPT
               PERFORM 4200-ACCRUE-INTEREST
               PERFORM 4210-FLAG-ITEM
               PERFORM 4220-FLAG-LINE
               PERFORM 4230-ASSESS-AUTO-RENEW
               PERFORM 4240-DESCRIBE-CHANNEL
               PERFORM 4250-CHECK-EFF
               PERFORM 4260-FLAG-PRINTED
               PERFORM 4270-CHECK-LAST-ACT
               PERFORM 4280-ACCRUE-PENALTY-PCT
               PERFORM 4290-ACCRUE-LINE
               PERFORM 4300-SPLIT-INTEREST
               PERFORM 4310-ACCUM-PRINTER
               PERFORM 4320-COUNT-ITEM
               PERFORM 4330-ASSESS-REVIEW
               PERFORM 4340-ACCRUE-LINE
               PERFORM 4350-LIMIT-PRINTED
               PERFORM 4360-COUNT-PAYMENT
               PERFORM 4370-AGE-OPEN
               PERFORM 4380-AGE-LAST-MAINT
               PERFORM 4390-COUNT-CYCLE
               PERFORM 4400-LIMIT-BALANCE
               PERFORM 4410-AGE-NEXT-REVIEW
               PERFORM 4420-CHECK-OPEN
               PERFORM 4430-COUNT-ITEM
               PERFORM 4440-SELECT-PENALTY-PCT
               PERFORM 4450-ACCUM-BRANCH
               PERFORM 4460-LIMIT-PRINTED
               PERFORM 4470-PRORATE-PRINTED
               PERFORM 4480-AGE-LAST-MAINT
               PERFORM 4490-LIMIT-PRINTED
               PERFORM 4500-EDIT-MEMO
               PERFORM 4510-CHECK-NEXT-REVIEW
               PERFORM 4520-DESCRIBE-PRINTER
               PERFORM 4530-SET-CHANNEL
               PERFORM 4540-LIMIT-LINE
               PERFORM 4550-ACCUM-BOOK-TYPE
               PERFORM 4560-ASSESS-HOLD
               PERFORM 4570-AGE-EFFECTIVE
               PERFORM 4580-FLAG-ITEM
               PERFORM 4590-EDIT-NAME
               PERFORM 4600-SPLIT-INTEREST
               PERFORM 4610-COUNT-CYCLE
               PERFORM 4620-SELECT-MARGIN
               PERFORM 4630-SPLIT-LINE
               PERFORM 4640-SET-BRANCH
               PERFORM 4650-LIMIT-CARRYFWD
               PERFORM 4660-LIMIT-PRINTED
               PERFORM 4670-COUNT-CYCLE
               PERFORM 4680-AGE-NEXT-DUE
               PERFORM 4690-LIMIT-UNPOSTED
               PERFORM 4700-ACCRUE-LINE
               PERFORM 4710-FLAG-ITEM
               PERFORM 4720-ACCRUE-CARRYFWD
               PERFORM 4730-ASSESS-SUSPEND
               PERFORM 4740-PRORATE-CARRYFWD
               PERFORM 4750-SPLIT-UNPOSTED
               PERFORM 4760-DESCRIBE-BRANCH
               PERFORM 4770-PRORATE-UNPOSTED
               PERFORM 4780-FLAG-PRINTED
               PERFORM 4790-COUNT-RETRY
               PERFORM 4800-ACCUM-PRINTER
               PERFORM 4810-CHECK-NEXT-REVIEW
               PERFORM 4820-ACCUM-BRANCH
               PERFORM 4830-ASSESS-AUTO-RENEW
               PERFORM 4840-SPLIT-INTEREST
               PERFORM 7020-CALL-PBSTAT
               PERFORM 7000-CALL-PBPRC
               REWRITE PBK-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PBKMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PBK-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD PBKM-LINE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-BALANCE.
           IF PBKM-BALANCE-YTD < 0.00
               MOVE 0.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-YTD > 1000.00
               MOVE 1000.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-PRINTED.
           IF PBKM-PRINTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-PRINTED-AMT * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-UNPOSTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3030-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN EFFECTIVE DATE IS PAST
      * D -> H WHEN EFFECTIVE DATE IS PAST
      * H -> L WHEN EFFECTIVE DATE IS PAST
      * L -> M WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PBKT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0 AND PBKM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN EXPIRY DATE IS PAST
      * D -> H WHEN EXPIRY DATE IS PAST
      * H -> L WHEN EXPIRY DATE IS PAST
      * L -> M WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-PRINTED.
           IF PBKM-PRINTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-PRINTED-AMT * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-BALANCE.
           IF PBKM-BALANCE-YTD < 100.00
               MOVE 100.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-YTD > 10000.00
               MOVE 10000.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-PRINTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-PRINTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'EP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E610 DESC REQUIRED
      * E011 LINE AMT NEGATIVE
      * E454 LINE AMT OVER 99999
      * E821 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3150-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT < 0
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT > 99999.00
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3160-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       PBKM-FEE-PCT / 200
               WHEN PBKT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN PBKT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E162 DESC REQUIRED
      * E707 WORK AMT 2 NEGATIVE
      * E863 WORK AMT 2 OVER 50000
      * E271 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN PBKM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-INTEREST.
           IF PBKT-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-INTEREST-AMT * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBKM-LINE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E544 CUST ID REQUIRED
      * E785 PRINTED AMT NEGATIVE
      * E819 PRINTED AMT OVER 10000
      * E780 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN PBKT-PRINTED-AMT < 0
                   MOVE 'E785' TO WS-REJECT-CODE
               WHEN PBKT-PRINTED-AMT > 10000.00
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN PBKM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-CARRYFWD-YTD * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-UNPOSTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE CARRYFWD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PBKT-CARRYFWD-AMT NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       3250-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-BALANCE-YTD ROUNDED =
                   PBKM-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-BALANCE-YTD
           END-IF
           ADD PBKM-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3260-FLAG-BALANCE.
           IF PBKM-BALANCE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3270-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE YTD.
      *----------------------------------------------------------------*
       3280-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-LINE-YTD ROUNDED =
                   PBKT-CARRYFWD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E876 OPERATOR REQUIRED
      * E133 LINE YTD NEGATIVE
      * E469 LINE YTD OVER 99999
      * E454 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3290-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-OPERATOR = SPACES
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD < 0
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD > 99999.00
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3300-LIMIT-CARRYFWD.
           IF PBKT-CARRYFWD-AMT < 0.00
               MOVE 0.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-CARRYFWD-AMT > 2500.00
               MOVE 2500.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINTED YTD.
      *----------------------------------------------------------------*
       3310-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-YTD ROUNDED =
                   PBKT-LINE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD AMT BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-PRINTER-CD
                   ADD PBKT-CARRYFWD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-CARRYFWD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-LINE-AMT ROUNDED =
                   PBKT-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKT-LINE-AMT
           END-IF
           ADD PBKT-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOOK TYPE CD:
      * CH -> HG WHEN CLOSE DATE IS PAST
      * HG -> KB WHEN CLOSE DATE IS PAST
      * KB -> KW WHEN CLOSE DATE IS PAST
      * KW -> PA WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HG' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HG'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KB' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KB'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PA' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PBKT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-UNPOSTED.
           IF PBKM-UNPOSTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-UNPOSTED-AMT * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-YTD - (WS-WORK-AMT * 4)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-BALANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD FEE ON THE BALANCE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3410-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-CARRYFWD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = PBKM-BALANCE-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 10.00 TO PBKM-CARRYFWD-AMT
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 15.00 TO PBKM-CARRYFWD-AMT
               WHEN OTHER
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = PBKM-BALANCE-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-CARRYFWD-AMT > 0 AND PBKM-CARRYFWD-AMT < 10.00
               MOVE 10.00 TO PBKM-CARRYFWD-AMT
           END-IF
           IF PBKM-CARRYFWD-AMT > 35.00
               MOVE 35.00 TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKT-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3440-FLAG-PRINTED.
           IF PBKM-PRINTED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-PRINTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-YTD - (WS-WORK-AMT * 4)
           COMPUTE PBKT-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-PRINTER-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKM-CARRYFWD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-CARRYFWD-YTD - (WS-WORK-AMT * 2)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3490-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-PRINTED.
           IF PBKM-PRINTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-PRINTED-YTD * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-LINE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E852 OPERATOR REQUIRED
      * E095 PRINTED YTD NEGATIVE
      * E826 PRINTED YTD OVER 99999
      * E356 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-OPERATOR = SPACES
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-YTD < 0
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-YTD > 99999.00
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINTED FEE ON THE UNPOSTED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3530-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBKT-PRINTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   COMPUTE PBKT-PRINTED-AMT ROUNDED = PBKM-UNPOSTED-AMT
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-BRANCH-CD = 'DF'
                   MOVE 20.00 TO PBKT-PRINTED-AMT
               WHEN PBKM-BRANCH-CD = 'NV'
                   MOVE 3.00 TO PBKT-PRINTED-AMT
               WHEN OTHER
                   COMPUTE PBKT-PRINTED-AMT ROUNDED = PBKM-UNPOSTED-AMT
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKT-PRINTED-AMT > 0 AND PBKT-PRINTED-AMT < 5.00
               MOVE 5.00 TO PBKT-PRINTED-AMT
           END-IF
           IF PBKT-PRINTED-AMT > 25.00
               MOVE 25.00 TO PBKT-PRINTED-AMT
           END-IF
           ADD PBKT-PRINTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E388 DESC REQUIRED
      * E354 PRINTED AMT NEGATIVE
      * E582 PRINTED AMT OVER 99999
      * E448 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN PBKT-PRINTED-AMT < 0
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN PBKT-PRINTED-AMT > 99999.00
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN MATURITY DATE IS PAST
      * EP -> EX WHEN MATURITY DATE IS PAST
      * EX -> JE WHEN MATURITY DATE IS PAST
      * JE -> MR WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-BALANCE.
           IF PBKM-BALANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-BALANCE-AMT * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-LINE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3580-FLAG-INTEREST.
           IF PBKM-INTEREST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3590-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBKM-LINE-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 20.00 TO PBKM-LINE-YTD
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 20.00 TO PBKM-LINE-YTD
               WHEN OTHER
                   COMPUTE PBKM-LINE-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-LINE-YTD > 0 AND PBKM-LINE-YTD < 10.00
               MOVE 10.00 TO PBKM-LINE-YTD
           END-IF
           IF PBKM-LINE-YTD > 35.00
               MOVE 35.00 TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3610-FLAG-INTEREST.
           IF PBKM-INTEREST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3620-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-PRINTER-CD
                   ADD PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E392 DESC REQUIRED
      * E288 UNPOSTED YTD NEGATIVE
      * E037 UNPOSTED YTD OVER 1000000
      * E511 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD < 0
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD > 1000000.00
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN PBKM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE YTD BY BOOK TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-BOOK-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BOOK-TYPE-CD
                   ADD PBKM-LINE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BOOK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3680-LIMIT-CARRYFWD.
           IF PBKT-CARRYFWD-AMT < 100.00
               MOVE 100.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-CARRYFWD-AMT > 5000.00
               MOVE 5000.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-TRAN-CD
                   ADD PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-CARRYFWD-AMT ROUNDED =
                   PBKM-UNPOSTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-BALANCE-AMT ROUNDED =
                   PBKT-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3720-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-INTEREST-AMT ROUNDED =
                   PBKM-UNPOSTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKT-INTEREST-AMT
           END-IF
           ADD PBKT-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD FEE ON THE PRINTED AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3730-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKT-CARRYFWD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBKT-CARRYFWD-AMT ROUNDED = PBKT-PRINTED-AMT
                       * PBKM-MARGIN / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 3.00 TO PBKT-CARRYFWD-AMT
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 20.00 TO PBKT-CARRYFWD-AMT
               WHEN OTHER
                   COMPUTE PBKT-CARRYFWD-AMT ROUNDED = PBKT-PRINTED-AMT
                       * PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKT-CARRYFWD-AMT > 0 AND PBKT-CARRYFWD-AMT < 2.00
               MOVE 2.00 TO PBKT-CARRYFWD-AMT
           END-IF
           IF PBKT-CARRYFWD-AMT > 50.00
               MOVE 50.00 TO PBKT-CARRYFWD-AMT
           END-IF
           ADD PBKT-CARRYFWD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3740-DESCRIBE-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LINE YTD.
      *----------------------------------------------------------------*
       3750-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-LINE-YTD ROUNDED =
                   PBKM-LINE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE PRINTED AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3760-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKM-PRINTED-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-PRINTER-CD = 'CB'
                   MOVE 7.00 TO PBKM-INTEREST-YTD
               WHEN PBKM-PRINTER-CD = 'EP'
                   MOVE 3.00 TO PBKM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKM-PRINTED-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-YTD > 0 AND PBKM-INTEREST-YTD < 1.00
               MOVE 1.00 TO PBKM-INTEREST-YTD
           END-IF
           IF PBKM-INTEREST-YTD > 75.00
               MOVE 75.00 TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-UNPOSTED.
           IF PBKM-UNPOSTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-UNPOSTED-AMT * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-LINE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKT-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKT-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-CARRYFWD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKT-PRINTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKT-PRINTED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-UNPOSTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E619 CONTACT REQUIRED
      * E873 WORK AMT NEGATIVE
      * E445 WORK AMT OVER 99999
      * E856 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CONTACT = SPACES
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3850-FLAG-INTEREST.
           IF PBKM-INTEREST-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBKM-INTEREST-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-REGION-CD
                   ADD PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN EFFECTIVE DATE IS PAST
      * NV -> PP WHEN EFFECTIVE DATE IS PAST
      * PP -> SH WHEN EFFECTIVE DATE IS PAST
      * SH -> ZP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3900-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-PRINTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-PRINTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E346 ADDR REQUIRED
      * E709 PRINTED AMT NEGATIVE
      * E877 PRINTED AMT OVER 10000
      * E015 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-ADDR = SPACES
                   MOVE 'E346' TO WS-REJECT-CODE
               WHEN PBKT-PRINTED-AMT < 0
                   MOVE 'E709' TO WS-REJECT-CODE
               WHEN PBKT-PRINTED-AMT > 10000.00
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN PBKM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-BALANCE.
           IF PBKM-BALANCE-AMT < 0.00
               MOVE 0.00 TO PBKM-BALANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-AMT > 1000.00
               MOVE 1000.00 TO PBKM-BALANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LINE SCHED TIER FOR THE CARRYFWD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LINE-SCHED-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-CARRYFWD-AMT NOT < WS-LINE-SCHED-LOW(WS-TX)
                   MOVE WS-LINE-SCHED-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-PRINTED.
           IF PBKM-PRINTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-PRINTED-YTD * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE PRINTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBKM-PRINTED-YTD NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-CARRYFWD.
           IF PBKT-CARRYFWD-AMT < 0.00
               MOVE 0.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-CARRYFWD-AMT > 10000.00
               MOVE 10000.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0 AND PBKM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
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
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN EFF DATE IS PAST
      * PR -> RB WHEN EFF DATE IS PAST
      * RB -> LS WHEN EFF DATE IS PAST
      * LS -> CF WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4010-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4020-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKM-LINE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN NEXT DUE DATE IS PAST
      * GL -> PE WHEN NEXT DUE DATE IS PAST
      * PE -> RJ WHEN NEXT DUE DATE IS PAST
      * RJ -> VB WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-TRAN-CD
                   ADD PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-YTD < 50.00
               MOVE 50.00 TO PBKM-CARRYFWD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-YTD > 5000.00
               MOVE 5000.00 TO PBKM-CARRYFWD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 10
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       4110-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-INTEREST-AMT ROUNDED =
                   PBKT-PRINTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKT-INTEREST-AMT
           END-IF
           ADD PBKT-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PBKT-INTEREST-AMT NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKM-CARRYFWD-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-CARRYFWD-AMT - (WS-WORK-AMT *
               12)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LINE SCHED TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-LINE-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-BALANCE-YTD NOT < WS-LINE-SCHED-LOW(WS-TX)
                   MOVE WS-LINE-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-YTD - (WS-WORK-AMT * 2)
           COMPUTE PBKT-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4170-FLAG-UNPOSTED.
           IF PBKM-UNPOSTED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-UNPOSTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CARRYFWD YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-CARRYFWD-YTD ROUNDED =
                   PBKM-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-CARRYFWD-YTD
           END-IF
           ADD PBKM-CARRYFWD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD YTD FEE ON THE LINE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4190-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-CARRYFWD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBKM-CARRYFWD-YTD ROUNDED = PBKM-LINE-AMT *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 7.00 TO PBKM-CARRYFWD-YTD
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 5.00 TO PBKM-CARRYFWD-YTD
               WHEN OTHER
                   COMPUTE PBKM-CARRYFWD-YTD ROUNDED = PBKM-LINE-AMT *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-CARRYFWD-YTD > 0 AND PBKM-CARRYFWD-YTD < 5.00
               MOVE 5.00 TO PBKM-CARRYFWD-YTD
           END-IF
           IF PBKM-CARRYFWD-YTD > 250.00
               MOVE 250.00 TO PBKM-CARRYFWD-YTD
           END-IF
           ADD PBKM-CARRYFWD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-INTEREST.
           IF PBKT-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-INTEREST-AMT * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-BALANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4210-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * LINE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4220-FLAG-LINE.
           IF PBKM-LINE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-LINE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE UNPOSTED YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4230-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   COMPUTE PBKM-BALANCE-AMT ROUNDED = PBKM-UNPOSTED-YTD
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 20.00 TO PBKM-BALANCE-AMT
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 3.00 TO PBKM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE PBKM-BALANCE-AMT ROUNDED = PBKM-UNPOSTED-YTD
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-BALANCE-AMT > 0 AND PBKM-BALANCE-AMT < 10.00
               MOVE 10.00 TO PBKM-BALANCE-AMT
           END-IF
           IF PBKM-BALANCE-AMT > 100.00
               MOVE 100.00 TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PBKT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4260-FLAG-PRINTED.
           IF PBKM-PRINTED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-PRINTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4270-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4280-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-LINE.
           IF PBKM-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-LINE-AMT * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-LINE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4300-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKT-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKT-INTEREST-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-UNPOSTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-PRINTER-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4320-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 10
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4330-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-BALANCE-AMT ROUNDED = PBKT-INTEREST-AMT
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-BOOK-TYPE-CD = 'CH'
                   MOVE 3.00 TO PBKM-BALANCE-AMT
               WHEN PBKM-BOOK-TYPE-CD = 'HG'
                   MOVE 5.00 TO PBKM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE PBKM-BALANCE-AMT ROUNDED = PBKT-INTEREST-AMT
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-BALANCE-AMT > 0 AND PBKM-BALANCE-AMT < 10.00
               MOVE 10.00 TO PBKM-BALANCE-AMT
           END-IF
           IF PBKM-BALANCE-AMT > 250.00
               MOVE 250.00 TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4340-ACCRUE-LINE.
           IF PBKT-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-LINE-AMT * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-LINE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4350-LIMIT-PRINTED.
           IF PBKT-PRINTED-AMT < 100.00
               MOVE 100.00 TO PBKT-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-PRINTED-AMT > 50000.00
               MOVE 50000.00 TO PBKT-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4360-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4380-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4390-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4400-LIMIT-BALANCE.
           IF PBKM-BALANCE-YTD < 50.00
               MOVE 50.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-YTD > 5000.00
               MOVE 5000.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0 AND PBKM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4420-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4430-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 5
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE CARRYFWD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBKT-CARRYFWD-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4450-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4460-LIMIT-PRINTED.
           IF PBKM-PRINTED-AMT < 25.00
               MOVE 25.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-AMT > 500.00
               MOVE 500.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4470-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PBKT-PRINTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4480-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4490-LIMIT-PRINTED.
           IF PBKM-PRINTED-AMT < 10.00
               MOVE 10.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-AMT > 50000.00
               MOVE 50000.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E939 MEMO REQUIRED
      * E982 INTEREST YTD NEGATIVE
      * E446 INTEREST YTD OVER 250000
      * E808 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4500-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-YTD < 0
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-YTD > 250000.00
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN PBKM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4520-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4530-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LINE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4540-LIMIT-LINE.
           IF PBKT-LINE-AMT < 10.00
               MOVE 10.00 TO PBKT-LINE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-LINE-AMT > 500.00
               MOVE 500.00 TO PBKT-LINE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY BOOK TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4550-ACCUM-BOOK-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BOOK-TYPE-CD
                   ADD PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BOOK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE PRINTED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4560-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKM-PRINTED-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 15.00 TO PBKM-INTEREST-YTD
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 10.00 TO PBKM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKM-PRINTED-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-YTD > 0 AND PBKM-INTEREST-YTD < 2.00
               MOVE 2.00 TO PBKM-INTEREST-YTD
           END-IF
           IF PBKM-INTEREST-YTD > 25.00
               MOVE 25.00 TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4570-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4580-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E606 NAME REQUIRED
      * E411 CARRYFWD AMT NEGATIVE
      * E545 CARRYFWD AMT OVER 25000
      * E120 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4590-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN PBKT-CARRYFWD-AMT < 0
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN PBKT-CARRYFWD-AMT > 25000.00
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4600-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE PBKT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4610-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4620-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBKM-BALANCE-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4630-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-BALANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN EFF DATE IS PAST
      * NV -> PP WHEN EFF DATE IS PAST
      * PP -> SH WHEN EFF DATE IS PAST
      * SH -> ZP WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4640-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4650-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-AMT < 0.00
               MOVE 0.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-AMT > 5000.00
               MOVE 5000.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4660-LIMIT-PRINTED.
           IF PBKM-PRINTED-AMT < 100.00
               MOVE 100.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-AMT > 5000.00
               MOVE 5000.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4670-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4680-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4690-LIMIT-UNPOSTED.
           IF PBKM-UNPOSTED-AMT < 50.00
               MOVE 50.00 TO PBKM-UNPOSTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-UNPOSTED-AMT > 2500.00
               MOVE 2500.00 TO PBKM-UNPOSTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4700-ACCRUE-LINE.
           IF PBKT-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-LINE-AMT * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4710-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4720-ACCRUE-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4730-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKT-CARRYFWD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKT-CARRYFWD-AMT ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKT-TRAN-CD = 'UP'
                   MOVE 3.00 TO PBKT-CARRYFWD-AMT
               WHEN PBKT-TRAN-CD = 'PR'
                   MOVE 20.00 TO PBKT-CARRYFWD-AMT
               WHEN OTHER
                   COMPUTE PBKT-CARRYFWD-AMT ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKT-CARRYFWD-AMT > 0 AND PBKT-CARRYFWD-AMT < 1.00
               MOVE 1.00 TO PBKT-CARRYFWD-AMT
           END-IF
           IF PBKT-CARRYFWD-AMT > 250.00
               MOVE 250.00 TO PBKT-CARRYFWD-AMT
           END-IF
           ADD PBKT-CARRYFWD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       4740-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-CARRYFWD-AMT ROUNDED =
                   PBKT-LINE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKT-CARRYFWD-AMT
           END-IF
           ADD PBKT-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4750-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBKM-UNPOSTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-UNPOSTED-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKM-PRINTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4760-DESCRIBE-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       4770-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-UNPOSTED-AMT ROUNDED =
                   PBKM-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-UNPOSTED-AMT
           END-IF
           ADD PBKM-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRINTED YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4780-FLAG-PRINTED.
           IF PBKM-PRINTED-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-PRINTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4790-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 10
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4800-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-PRINTER-CD
                   ADD PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4810-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4820-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE LINE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4830-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKT-LINE-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 7.00 TO PBKM-INTEREST-YTD
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 20.00 TO PBKM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKT-LINE-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-YTD > 0 AND PBKM-INTEREST-YTD < 10.00
               MOVE 10.00 TO PBKM-INTEREST-YTD
           END-IF
           IF PBKM-INTEREST-YTD > 150.00
               MOVE 150.00 TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4840-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKT-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKT-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-PBSTAT.
           INITIALIZE PBSTAT-PARMS
           MOVE PBKM-UNPOSTED-YTD
             TO PBSTAT-UNPOSTED-YTD
           MOVE PBKM-BALANCE-AMT
             TO PBSTAT-BALANCE-AMT
           MOVE PBKM-LINE-YTD
             TO PBSTAT-LINE-YTD
           MOVE PBKM-CARRYFWD-YTD
             TO PBSTAT-CARRYFWD-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBSTAT-UNPOSTED-AMT
           MOVE PBKM-LINE-AMT
             TO PBSTAT-LINE-AMT
           MOVE PBKM-CLOSE-DATE
             TO PBSTAT-CLOSE-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBSTAT-EFFECTIVE-DATE
           MOVE PBKM-NEXT-DUE-DATE
             TO PBSTAT-NEXT-DUE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBSTAT-NEXT-REVIEW-DATE
           MOVE PBKM-BOOK-TYPE-CD
             TO PBSTAT-BOOK-TYPE-CD
           MOVE PBKM-PRINTER-CD
             TO PBSTAT-PRINTER-CD
           MOVE PBKM-REGION-CD
             TO PBSTAT-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBSTAT-SOURCE-CD
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBSTAT-NOTICE-SENT-FLAG
           MOVE PBKM-REVIEW-FLAG
             TO PBSTAT-REVIEW-FLAG
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBSTAT-AUTO-RENEW-FLAG
           MOVE PBKM-RETRY-CNT
             TO PBSTAT-RETRY-CNT
           MOVE PBKM-CYCLE-CNT
             TO PBSTAT-CYCLE-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBSTAT-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBSTAT-MARGIN
           MOVE PBKM-CONTACT
             TO PBSTAT-CONTACT
           MOVE PBKM-CUST-ID
             TO PBSTAT-CUST-ID
           CALL 'PBSTAT' USING PBSTAT-PARMS
           EVALUATE PBSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBSTAT-NOTICE-SENT-FLAG
                     TO PBKM-NOTICE-SENT-FLAG
                   MOVE PBSTAT-REVIEW-FLAG
                     TO PBKM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PBSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-PBPRC.
           INITIALIZE PBPRC-PARMS
           MOVE PBKM-PRINTED-AMT
             TO PBPRC-PRINTED-AMT
           MOVE PBKM-LINE-YTD
             TO PBPRC-LINE-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBPRC-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBPRC-CARRYFWD-YTD
           MOVE PBKM-INTEREST-AMT
             TO PBPRC-INTEREST-AMT
           MOVE PBKM-INTEREST-YTD
             TO PBPRC-INTEREST-YTD
           MOVE PBKM-MATURITY-DATE
             TO PBPRC-MATURITY-DATE
           MOVE PBKM-EXPIRY-DATE
             TO PBPRC-EXPIRY-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBPRC-EFFECTIVE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBPRC-NEXT-REVIEW-DATE
           MOVE PBKM-CHANNEL-CD
             TO PBPRC-CHANNEL-CD
           MOVE PBKM-REGION-CD
             TO PBPRC-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBPRC-SOURCE-CD
           MOVE PBKM-PRINTER-CD
             TO PBPRC-PRINTER-CD
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBPRC-AUTO-RENEW-FLAG
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBPRC-NOTICE-SENT-FLAG
           MOVE PBKM-HOLD-FLAG
             TO PBPRC-HOLD-FLAG
           MOVE PBKM-PAYMENT-CNT
             TO PBPRC-PAYMENT-CNT
           MOVE PBKM-ITEM-CNT
             TO PBPRC-ITEM-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBPRC-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBPRC-MARGIN
           MOVE PBKM-CUST-ID
             TO PBPRC-CUST-ID
           MOVE PBKM-ADDR
             TO PBPRC-ADDR
           CALL 'PBPRC' USING PBPRC-PARMS
           EVALUATE PBPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBPRC-PRINTED-AMT
                     TO PBKM-PRINTED-AMT
                   MOVE PBPRC-LINE-YTD
                     TO PBKM-LINE-YTD
                   MOVE PBPRC-CARRYFWD-YTD
                     TO PBKM-CARRYFWD-YTD
                   MOVE PBPRC-INTEREST-AMT
                     TO PBKM-INTEREST-AMT
                   MOVE PBPRC-INTEREST-YTD
                     TO PBKM-INTEREST-YTD
                   MOVE PBPRC-AUTO-RENEW-FLAG
                     TO PBKM-AUTO-RENEW-FLAG
                   MOVE PBPRC-HOLD-FLAG
                     TO PBKM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-PBPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PBKT-KEY TO RD-KEY
           MOVE PBKT-TRAN-CD TO RD-CODE
           MOVE PBKT-PRINTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PBKT-KEY TO RD-KEY
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
           DISPLAY 'PBKSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PBKSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
