      *================================================================*
      * PROGRAM  : PBKXMIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PASSBOOK PRINTING - OUTBOUND TRANSMISSION. READS THE*
      *            PBK TRANSACTIONS, UPDATES THE PBK MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. PASSBOOK SAVINGS UPDATE*
      *            PRINTING AND UNPOSTED ITEM CONTROL                  *
      * INPUT    : PBKMST (KSDS), PBKTRN, BANKCTL                      *
      * OUTPUT   : PBKOUT, PBKXMRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PBDTE, PBSTAT           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-09-10 DPS  ORIGINAL                                       *
      * 2002-02-27 TWH  PERFORMANCE - INTEREST READ AHEAD PRB1035      *
      * 2002-05-08 JLM  BALANCE RERUN PROTECTION INC0968               *
      * 2006-04-21 EWS  BALANCE NOTICE TEXT PER LEGAL INC9524          *
      * 2011-10-25 GAF  BALANCE CUTOFF MOVED PRB3567                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBKXMIT.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "PBKXMRPT"
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
                                           VALUE 19.000000.
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
      * WS-UNPOSTED-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-UNPOSTED-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000299000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000093500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000571000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000441500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011150000000569500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012150000000238000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022150000000744000'.
       01  WS-UNPOSTED-BAND-TBL REDEFINES WS-UNPOSTED-BAND-VALUES.
           05  WS-UNPOSTED-BAND-ROW OCCURS 7 TIMES.
               10  WS-UNPOSTED-BAND-LOW    PIC 9(09)V99.
               10  WS-UNPOSTED-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CARRYFWD-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CARRYFWD-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000662500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000312000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000503500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000477500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000821500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000663500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023100000000278000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023350000000670500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024350000000397500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024850000000593000'.
       01  WS-CARRYFWD-BAND-TBL REDEFINES WS-CARRYFWD-BAND-VALUES.
           05  WS-CARRYFWD-BAND-ROW OCCURS 10 TIMES.
               10  WS-CARRYFWD-BAND-LOW    PIC 9(09)V99.
               10  WS-CARRYFWD-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PBKXMIT  PASSBOOK PRINTING'.
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
       COPY PBDTEP.
       COPY PBSTATP.

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
               PERFORM 3000-SPLIT-PRINTED
               PERFORM 3010-ACCUM-PRINTER
               PERFORM 3020-SELECT-FEE-PCT
               PERFORM 3030-FLAG-INTEREST
               PERFORM 3040-AGE-NEXT-DUE
               PERFORM 3050-AGE-EFF
               PERFORM 3060-SET-BOOK-TYPE
               PERFORM 3070-LIMIT-LINE
               PERFORM 3080-LIMIT-BALANCE
               PERFORM 3090-ACCRUE-INTEREST
               PERFORM 3100-ACCUM-BRANCH
               PERFORM 3110-SPLIT-PRINTED
               PERFORM 3120-SELECT-FEE-PCT
               PERFORM 3130-COUNT-RETRY
               PERFORM 3140-ACCUM-CHANNEL
               PERFORM 3150-SET-REGION
               PERFORM 3160-ACCRUE-INTEREST
               PERFORM 3170-CHECK-LAST-MAINT
               PERFORM 3180-EDIT-DESC
               PERFORM 3190-EDIT-CUST-ID
               PERFORM 3200-AGE-LAST-MAINT
               PERFORM 3210-ACCRUE-PRINTED
               PERFORM 3220-COUNT-RETRY
               PERFORM 3230-FLAG-LINE
               PERFORM 3240-ACCRUE-BALANCE
               PERFORM 3250-SPLIT-LINE
               PERFORM 3260-EDIT-REJECT
               PERFORM 3270-FLAG-CARRYFWD
               PERFORM 3280-DESCRIBE-PRINTER
               PERFORM 3290-CHECK-MATURITY
               PERFORM 3300-AGE-EFFECTIVE
               PERFORM 3310-SPLIT-PRINTED
               PERFORM 3320-ACCUM-TRAN
               PERFORM 3330-CHECK-NEXT-REVIEW
               PERFORM 3340-LIMIT-PRINTED
               PERFORM 3350-SPLIT-UNPOSTED
               PERFORM 3360-ACCRUE-LINE
               PERFORM 3370-ACCRUE-BALANCE
               PERFORM 3380-COUNT-ITEM
               PERFORM 3390-CHECK-POST
               PERFORM 3400-COUNT-ITEM
               PERFORM 3410-SET-CHANNEL
               PERFORM 3420-EDIT-MEMO
               PERFORM 3430-COUNT-RETRY
               PERFORM 3440-DESCRIBE-BOOK-TYPE
               PERFORM 3450-LIMIT-LINE
               PERFORM 3460-DESCRIBE-SOURCE
               PERFORM 3470-CHECK-EFF
               PERFORM 3480-PRORATE-INTEREST
               PERFORM 3490-SELECT-MARGIN
               PERFORM 3500-DESCRIBE-PRINTER
               PERFORM 3510-ACCRUE-INTEREST
               PERFORM 3520-SELECT-FEE-PCT
               PERFORM 3530-PRORATE-INTEREST
               PERFORM 3540-SELECT-MARGIN
               PERFORM 3550-PRORATE-LINE
               PERFORM 3560-SPLIT-BALANCE
               PERFORM 3570-AGE-EFF
               PERFORM 3580-FLAG-INTEREST
               PERFORM 3590-PRORATE-BALANCE
               PERFORM 3600-CHECK-EFFECTIVE
               PERFORM 3610-PRORATE-CARRYFWD
               PERFORM 3620-AGE-OPEN
               PERFORM 3630-DESCRIBE-PRINTER
               PERFORM 3640-SET-BOOK-TYPE
               PERFORM 3650-FLAG-INTEREST
               PERFORM 3660-LIMIT-CARRYFWD
               PERFORM 3670-DESCRIBE-TRAN
               PERFORM 3680-SELECT-FEE-PCT
               PERFORM 3690-ACCRUE-PRINTED
               PERFORM 3700-COUNT-PAYMENT
               PERFORM 3710-AGE-LAST-ACT
               PERFORM 3720-SPLIT-LINE
               PERFORM 3730-ASSESS-REVIEW
               PERFORM 3740-FLAG-CARRYFWD
               PERFORM 3750-AGE-POST
               PERFORM 3760-PRORATE-CARRYFWD
               PERFORM 3770-ASSESS-SUSPEND
               PERFORM 3780-ACCUM-CHANNEL
               PERFORM 3790-CHECK-OPEN
               PERFORM 3800-AGE-NEXT-REVIEW
               PERFORM 3810-EDIT-CUST-ID
               PERFORM 3820-FLAG-INTEREST
               PERFORM 3830-ASSESS-AUTO-RENEW
               PERFORM 3840-AGE-NEXT-DUE
               PERFORM 3850-SELECT-FEE-PCT
               PERFORM 3860-ASSESS-REVERSAL
               PERFORM 3870-SELECT-PENALTY-PCT
               PERFORM 3880-COUNT-SEQ
               PERFORM 3890-LIMIT-INTEREST
               PERFORM 3900-ASSESS-EXEMPT
               PERFORM 3910-ACCRUE-LINE
               PERFORM 3920-EDIT-DESC
               PERFORM 3930-SPLIT-LINE
               PERFORM 3940-ACCUM-BOOK-TYPE
               PERFORM 3950-ACCRUE-MARGIN
               PERFORM 3960-SPLIT-INTEREST
               PERFORM 3970-PRORATE-LINE
               PERFORM 3980-AGE-LAST-MAINT
               PERFORM 3990-ACCRUE-PRINTED
               PERFORM 4000-ASSESS-REVIEW
               PERFORM 4010-EDIT-OPERATOR
               PERFORM 4020-PRORATE-CARRYFWD
               PERFORM 4030-ACCUM-BRANCH
               PERFORM 4040-ACCUM-REGION
               PERFORM 4050-FLAG-ITEM
               PERFORM 4060-EDIT-DESC
               PERFORM 4070-SPLIT-INTEREST
               PERFORM 4080-ACCUM-BRANCH
               PERFORM 4090-EDIT-CUST-ID
               PERFORM 4100-SPLIT-BALANCE
               PERFORM 4110-ACCUM-SOURCE
               PERFORM 4120-DESCRIBE-PRINTER
               PERFORM 4130-SPLIT-BALANCE
               PERFORM 4140-CHECK-MATURITY
               PERFORM 4150-PRORATE-INTEREST
               PERFORM 4160-ASSESS-REVIEW
               PERFORM 4170-SPLIT-BALANCE
               PERFORM 4180-CHECK-MATURITY
               PERFORM 4190-SET-CHANNEL
               PERFORM 4200-SPLIT-BALANCE
               PERFORM 4210-SET-PRINTER
               PERFORM 4220-ASSESS-NOTICE-SENT
               PERFORM 4230-SELECT-FEE-PCT
               PERFORM 4240-DESCRIBE-PRINTER
               PERFORM 4250-FLAG-BALANCE
               PERFORM 4260-COUNT-CYCLE
               PERFORM 4270-PRORATE-UNPOSTED
               PERFORM 4280-SET-REGION
               PERFORM 4290-ACCRUE-CARRYFWD
               PERFORM 4300-SET-CHANNEL
               PERFORM 4310-CHECK-OPEN
               PERFORM 4320-ASSESS-NOTICE-SENT
               PERFORM 4330-ASSESS-SUSPEND
               PERFORM 4340-SET-CHANNEL
               PERFORM 4350-COUNT-ITEM
               PERFORM 4360-CHECK-EXPIRY
               PERFORM 4370-DESCRIBE-REGION
               PERFORM 4380-EDIT-OPERATOR
               PERFORM 4390-PRORATE-PRINTED
               PERFORM 4400-DESCRIBE-PRINTER
               PERFORM 4410-SPLIT-LINE
               PERFORM 4420-SELECT-PENALTY-PCT
               PERFORM 4430-FLAG-CARRYFWD
               PERFORM 4440-AGE-EFF
               PERFORM 4450-LIMIT-CARRYFWD
               PERFORM 4460-DESCRIBE-REGION
               PERFORM 4470-SPLIT-PRINTED
               PERFORM 4480-AGE-EFF
               PERFORM 4490-PRORATE-INTEREST
               PERFORM 4500-SELECT-FEE-PCT
               PERFORM 4510-ACCUM-CHANNEL
               PERFORM 4520-AGE-EFF
               PERFORM 4530-LIMIT-INTEREST
               PERFORM 4540-SET-TRAN
               PERFORM 4550-AGE-OPEN
               PERFORM 4560-COUNT-ITEM
               PERFORM 4570-ACCRUE-UNPOSTED
               PERFORM 4580-COUNT-SEQ
               PERFORM 4590-SPLIT-BALANCE
               PERFORM 4600-AGE-POST
               PERFORM 4610-SPLIT-CARRYFWD
               PERFORM 4620-ASSESS-HOLD
               PERFORM 4630-EDIT-REJECT
               PERFORM 4640-FLAG-CARRYFWD
               PERFORM 4650-PRORATE-INTEREST
               PERFORM 4660-FLAG-CARRYFWD
               PERFORM 4670-ACCUM-TRAN
               PERFORM 4680-ACCUM-REGION
               PERFORM 4690-SET-REGION
               PERFORM 4700-AGE-NEXT-DUE
               PERFORM 4710-SET-TRAN
               PERFORM 4720-COUNT-ITEM
               PERFORM 4730-ACCUM-PRINTER
               PERFORM 4740-LIMIT-CARRYFWD
               PERFORM 4750-SPLIT-CARRYFWD
               PERFORM 4760-COUNT-ITEM
               PERFORM 4770-DESCRIBE-PRINTER
               PERFORM 4780-LIMIT-PRINTED
               PERFORM 4790-COUNT-CYCLE
               PERFORM 4800-AGE-LAST-MAINT
               PERFORM 4810-CHECK-CLOSE
               PERFORM 7010-CALL-PBDTE
               PERFORM 7020-CALL-PBSTAT
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
      * SPLIT PRINTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKT-PRINTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKT-PRINTED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKT-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-PRINTER-CD
                   ADD PBKM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKT-INTEREST-AMT NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3030-FLAG-INTEREST.
           IF PBKM-INTEREST-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-NEXT-DUE.
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
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-EFF.
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
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOOK TYPE CD:
      * CH -> HG WHEN LAST ACT DATE IS PAST
      * HG -> KB WHEN LAST ACT DATE IS PAST
      * KB -> KW WHEN LAST ACT DATE IS PAST
      * KW -> PA WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HG' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HG'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KB' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KB'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PA' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LINE YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-LINE.
           IF PBKM-LINE-YTD < 100.00
               MOVE 100.00 TO PBKM-LINE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-LINE-YTD > 1000.00
               MOVE 1000.00 TO PBKM-LINE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3080-LIMIT-BALANCE.
           IF PBKM-BALANCE-YTD < 10.00
               MOVE 10.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-YTD > 2500.00
               MOVE 2500.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-INTEREST.
           IF PBKM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-YTD * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-BALANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKT-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKT-PRINTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKT-PRINTED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED BAND TIER FOR THE UNPOSTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-BAND-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBKM-UNPOSTED-YTD NOT < WS-UNPOSTED-BAND-LOW(WS-TX)
                   MOVE WS-UNPOSTED-BAND-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 12
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-CHANNEL.
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
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN EXPIRY DATE IS PAST
      * GL -> PE WHEN EXPIRY DATE IS PAST
      * PE -> RJ WHEN EXPIRY DATE IS PAST
      * RJ -> VB WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-INTEREST.
           IF PBKT-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-INTEREST-AMT * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKT-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E138 DESC REQUIRED
      * E335 LINE AMT NEGATIVE
      * E855 LINE AMT OVER 25000
      * E895 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT < 0
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT > 25000.00
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E895' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E935 CUST ID REQUIRED
      * E204 WORK AMT NEGATIVE
      * E188 WORK AMT OVER 10000
      * E115 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN PBKT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E115' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-LAST-MAINT.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-PRINTED.
           IF PBKM-PRINTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-PRINTED-YTD * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-UNPOSTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 10
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LINE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3230-FLAG-LINE.
           IF PBKM-LINE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-BALANCE.
           IF PBKM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-BALANCE-YTD * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-YTD - (WS-WORK-AMT * 2)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E227 REJECT CD REQUIRED
      * E518 LINE YTD NEGATIVE
      * E233 LINE YTD OVER 10000
      * E801 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-REJECT-CD = SPACES
                   MOVE 'E227' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD < 0
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD > 10000.00
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN PBKM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3270-FLAG-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKT-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-MATURITY.
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
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-EFFECTIVE.
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
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-TRAN-CD
                   ADD PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-PRINTED.
           IF PBKM-PRINTED-YTD < 0.00
               MOVE 0.00 TO PBKM-PRINTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-YTD > 50000.00
               MOVE 50000.00 TO PBKM-PRINTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBKM-UNPOSTED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-UNPOSTED-YTD - (WS-WORK-AMT * 2)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-LINE.
           IF PBKT-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-LINE-AMT * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-BALANCE.
           IF PBKM-BALANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-BALANCE-AMT * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-LINE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 10
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-POST.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 12
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-CHANNEL.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 MEMO REQUIRED
      * E879 UNPOSTED YTD NEGATIVE
      * E931 UNPOSTED YTD OVER 1000000
      * E596 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD < 0
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD > 1000000.00
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 3
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LINE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3450-LIMIT-LINE.
           IF PBKM-LINE-AMT < 25.00
               MOVE 25.00 TO PBKM-LINE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-LINE-AMT > 1000.00
               MOVE 1000.00 TO PBKM-LINE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-EFF.
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
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3480-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-INTEREST-YTD ROUNDED =
                   PBKT-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED BAND TIER FOR THE UNPOSTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3490-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-BAND-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBKM-UNPOSTED-YTD NOT < WS-UNPOSTED-BAND-LOW(WS-TX)
                   MOVE WS-UNPOSTED-BAND-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'EP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-INTEREST.
           IF PBKM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-AMT * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED BAND TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-BAND-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBKT-LINE-AMT NOT < WS-UNPOSTED-BAND-LOW(WS-TX)
                   MOVE WS-UNPOSTED-BAND-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-INTEREST-AMT ROUNDED =
                   PBKT-PRINTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKT-INTEREST-AMT
           END-IF
           ADD PBKT-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE LINE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKM-LINE-YTD NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-LINE-AMT ROUNDED =
                   PBKT-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-LINE-AMT
           END-IF
           ADD PBKM-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT BALANCE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-YTD - (WS-WORK-AMT * 12)
           COMPUTE PBKM-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-EFF.
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
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-UNPOSTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3580-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE AMT.
      *----------------------------------------------------------------*
       3590-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-BALANCE-AMT ROUNDED =
                   PBKT-LINE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CARRYFWD YTD.
      *----------------------------------------------------------------*
       3610-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-CARRYFWD-YTD ROUNDED =
                   PBKM-CARRYFWD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-CARRYFWD-YTD
           END-IF
           ADD PBKM-CARRYFWD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3620-AGE-OPEN.
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
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOOK TYPE CD:
      * CH -> HG WHEN OPEN DATE IS PAST
      * HG -> KB WHEN OPEN DATE IS PAST
      * KB -> KW WHEN OPEN DATE IS PAST
      * KW -> PA WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HG' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HG'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KB' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KB'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PA' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3650-FLAG-INTEREST.
           IF PBKM-INTEREST-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3660-LIMIT-CARRYFWD.
           IF PBKT-CARRYFWD-AMT < 100.00
               MOVE 100.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-CARRYFWD-AMT > 2500.00
               MOVE 2500.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKM-INTEREST-AMT NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-PRINTED.
           IF PBKT-PRINTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-PRINTED-AMT * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-PRINTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LINE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKT-LINE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKT-LINE-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-CARRYFWD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE INTEREST YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3730-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-AMT ROUNDED = PBKM-INTEREST-YTD
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-PRINTER-CD = 'CB'
                   MOVE 12.00 TO PBKM-INTEREST-AMT
               WHEN PBKM-PRINTER-CD = 'EP'
                   MOVE 20.00 TO PBKM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-AMT ROUNDED = PBKM-INTEREST-YTD
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-AMT > 0 AND PBKM-INTEREST-AMT < 1.00
               MOVE 1.00 TO PBKM-INTEREST-AMT
           END-IF
           IF PBKM-INTEREST-AMT > 150.00
               MOVE 150.00 TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3740-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-CARRYFWD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE LINE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3770-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKT-LINE-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-BRANCH-CD = 'DF'
                   MOVE 10.00 TO PBKM-INTEREST-YTD
               WHEN PBKM-BRANCH-CD = 'NV'
                   MOVE 12.00 TO PBKM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKT-LINE-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-YTD > 0 AND PBKM-INTEREST-YTD < 2.00
               MOVE 2.00 TO PBKM-INTEREST-YTD
           END-IF
           IF PBKM-INTEREST-YTD > 25.00
               MOVE 25.00 TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-OPEN.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-NEXT-REVIEW.
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
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E815 CUST ID REQUIRED
      * E533 LINE AMT NEGATIVE
      * E234 LINE AMT OVER 50000
      * E789 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT < 0
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT > 50000.00
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3820-FLAG-INTEREST.
           IF PBKM-INTEREST-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3830-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-LINE-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 20.00 TO PBKM-LINE-YTD
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 12.00 TO PBKM-LINE-YTD
               WHEN OTHER
                   COMPUTE PBKM-LINE-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-LINE-YTD > 0 AND PBKM-LINE-YTD < 5.00
               MOVE 5.00 TO PBKM-LINE-YTD
           END-IF
           IF PBKM-LINE-YTD > 35.00
               MOVE 35.00 TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-NEXT-DUE.
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
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE UNPOSTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKM-UNPOSTED-YTD NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3860-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE PBKM-LINE-AMT ROUNDED = PBKT-CARRYFWD-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-BOOK-TYPE-CD = 'CH'
                   MOVE 5.00 TO PBKM-LINE-AMT
               WHEN PBKM-BOOK-TYPE-CD = 'HG'
                   MOVE 12.00 TO PBKM-LINE-AMT
               WHEN OTHER
                   COMPUTE PBKM-LINE-AMT ROUNDED = PBKT-CARRYFWD-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-LINE-AMT > 0 AND PBKM-LINE-AMT < 10.00
               MOVE 10.00 TO PBKM-LINE-AMT
           END-IF
           IF PBKM-LINE-AMT > 50.00
               MOVE 50.00 TO PBKM-LINE-AMT
           END-IF
           ADD PBKM-LINE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE LINE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKM-LINE-YTD NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 5
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-INTEREST.
           IF PBKM-INTEREST-YTD < 25.00
               MOVE 25.00 TO PBKM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-INTEREST-YTD > 1000.00
               MOVE 1000.00 TO PBKM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE FEE ON THE INTEREST AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3900-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBKM-LINE-AMT ROUNDED = PBKT-INTEREST-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 5.00 TO PBKM-LINE-AMT
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 20.00 TO PBKM-LINE-AMT
               WHEN OTHER
                   COMPUTE PBKM-LINE-AMT ROUNDED = PBKT-INTEREST-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-LINE-AMT > 0 AND PBKM-LINE-AMT < 2.00
               MOVE 2.00 TO PBKM-LINE-AMT
           END-IF
           IF PBKM-LINE-AMT > 75.00
               MOVE 75.00 TO PBKM-LINE-AMT
           END-IF
           ADD PBKM-LINE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-LINE.
           IF PBKT-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-LINE-AMT * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E145 DESC REQUIRED
      * E165 WORK AMT NEGATIVE
      * E032 WORK AMT OVER 25000
      * E047 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E032' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKT-LINE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKT-LINE-AMT - (WS-WORK-AMT * 12)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY BOOK TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-BOOK-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BOOK-TYPE-CD
                   ADD PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BOOK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKM-INTEREST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-INTEREST-YTD - (WS-WORK-AMT * 4)
           COMPUTE PBKM-LINE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-LINE-AMT ROUNDED =
                   PBKM-PRINTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKT-LINE-AMT
           END-IF
           ADD PBKT-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-LAST-MAINT.
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
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-PRINTED.
           IF PBKT-PRINTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-PRINTED-AMT * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBKM-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD FEE ON THE LINE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4000-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-CARRYFWD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = PBKT-LINE-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 10.00 TO PBKM-CARRYFWD-AMT
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 10.00 TO PBKM-CARRYFWD-AMT
               WHEN OTHER
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = PBKT-LINE-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-CARRYFWD-AMT > 0 AND PBKM-CARRYFWD-AMT < 10.00
               MOVE 10.00 TO PBKM-CARRYFWD-AMT
           END-IF
           IF PBKM-CARRYFWD-AMT > 35.00
               MOVE 35.00 TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E652 OPERATOR REQUIRED
      * E712 BALANCE AMT NEGATIVE
      * E712 BALANCE AMT OVER 10000
      * E142 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-OPERATOR = SPACES
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-AMT < 0
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-AMT > 10000.00
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN PBKM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       4020-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-CARRYFWD-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKT-CARRYFWD-AMT
           END-IF
           ADD PBKT-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-REGION-CD
                   ADD PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4050-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E382 DESC REQUIRED
      * E409 UNPOSTED YTD NEGATIVE
      * E473 UNPOSTED YTD OVER 50000
      * E898 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E382' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD < 0
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD > 50000.00
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKM-INTEREST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-INTEREST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE PBKT-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E156 CUST ID REQUIRED
      * E078 WORK AMT 2 NEGATIVE
      * E391 WORK AMT 2 OVER 5000
      * E409 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-SOURCE-CD
                   ADD PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-AMT - (WS-WORK-AMT * 12)
           COMPUTE PBKM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-MATURITY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-INTEREST-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE LINE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4160-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-AMT ROUNDED = PBKM-LINE-YTD *
                       PBKM-MARGIN / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 5.00 TO PBKM-INTEREST-AMT
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 10.00 TO PBKM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-AMT ROUNDED = PBKM-LINE-YTD *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-INTEREST-AMT > 0 AND PBKM-INTEREST-AMT < 5.00
               MOVE 5.00 TO PBKM-INTEREST-AMT
           END-IF
           IF PBKM-INTEREST-AMT > 75.00
               MOVE 75.00 TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-BALANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-MATURITY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-AMT - (WS-WORK-AMT * 12)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN MATURITY DATE IS PAST
      * EP -> EX WHEN MATURITY DATE IS PAST
      * EX -> JE WHEN MATURITY DATE IS PAST
      * JE -> MR WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-PRINTER.
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
      * ASSESS THE WORK FEE ON THE CARRYFWD YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4220-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PBKM-CARRYFWD-YTD *
                       PBKM-MARGIN / 200
               WHEN PBKT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN PBKT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PBKM-CARRYFWD-YTD *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4250-FLAG-BALANCE.
           IF PBKM-BALANCE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO UNPOSTED YTD.
      *----------------------------------------------------------------*
       4270-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-UNPOSTED-YTD ROUNDED =
                   PBKM-UNPOSTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-UNPOSTED-YTD
           END-IF
           ADD PBKM-UNPOSTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN NEXT DUE DATE IS PAST
      * GL -> PE WHEN NEXT DUE DATE IS PAST
      * PE -> RJ WHEN NEXT DUE DATE IS PAST
      * RJ -> VB WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4280-SET-REGION.
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
      * DAILY INTEREST ON CARRYFWD AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-CHANNEL.
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
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-OPEN.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE YTD FEE ON THE CARRYFWD YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4320-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-LINE-YTD ROUNDED = PBKM-CARRYFWD-YTD *
                       PBKM-MARGIN / 200
               WHEN PBKT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO PBKM-LINE-YTD
               WHEN PBKT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO PBKM-LINE-YTD
               WHEN OTHER
                   COMPUTE PBKM-LINE-YTD ROUNDED = PBKM-CARRYFWD-YTD *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-LINE-YTD > 0 AND PBKM-LINE-YTD < 2.00
               MOVE 2.00 TO PBKM-LINE-YTD
           END-IF
           IF PBKM-LINE-YTD > 25.00
               MOVE 25.00 TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE LINE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4330-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PBKT-LINE-AMT *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PBKT-LINE-AMT *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4340-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 6
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4360-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EXPIRY-DATE TO DTU-DATE-1
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
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4370-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E753 OPERATOR REQUIRED
      * E798 PRINTED AMT NEGATIVE
      * E541 PRINTED AMT OVER 5000
      * E209 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4380-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-OPERATOR = SPACES
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-AMT < 0
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-AMT > 5000.00
                   MOVE 'E541' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       4390-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-PRINTED-AMT ROUNDED =
                   PBKM-UNPOSTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKT-PRINTED-AMT
           END-IF
           ADD PBKT-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4400-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LINE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-YTD - (WS-WORK-AMT * 4)
           COMPUTE PBKM-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE PRINTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4420-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKM-PRINTED-YTD NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4430-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-EFF.
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
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4450-LIMIT-CARRYFWD.
           IF PBKT-CARRYFWD-AMT < 0.00
               MOVE 0.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-CARRYFWD-AMT > 2500.00
               MOVE 2500.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4460-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-PRINTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4480-AGE-EFF.
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
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4490-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE CARRYFWD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4500-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PBKM-CARRYFWD-YTD NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4510-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-EFF.
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
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4530-LIMIT-INTEREST.
           IF PBKT-INTEREST-AMT < 50.00
               MOVE 50.00 TO PBKT-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-INTEREST-AMT > 5000.00
               MOVE 5000.00 TO PBKT-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN MATURITY DATE IS PAST
      * PR -> RB WHEN MATURITY DATE IS PAST
      * RB -> LS WHEN MATURITY DATE IS PAST
      * LS -> CF WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4540-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4550-AGE-OPEN.
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
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4560-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 5
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4570-ACCRUE-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-UNPOSTED-YTD * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4580-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 6
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4590-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-YTD - (WS-WORK-AMT * 2)
           COMPUTE PBKM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4600-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-CARRYFWD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4610-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKT-CARRYFWD-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKT-CARRYFWD-AMT - (WS-WORK-AMT *
               12)
           COMPUTE PBKM-BALANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE CARRYFWD YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4620-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   COMPUTE PBKM-BALANCE-YTD ROUNDED = PBKM-CARRYFWD-YTD
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 15.00 TO PBKM-BALANCE-YTD
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 5.00 TO PBKM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE PBKM-BALANCE-YTD ROUNDED = PBKM-CARRYFWD-YTD
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-BALANCE-YTD > 0 AND PBKM-BALANCE-YTD < 2.00
               MOVE 2.00 TO PBKM-BALANCE-YTD
           END-IF
           IF PBKM-BALANCE-YTD > 100.00
               MOVE 100.00 TO PBKM-BALANCE-YTD
           END-IF
           ADD PBKM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E166 REJECT CD REQUIRED
      * E239 INTEREST AMT NEGATIVE
      * E727 INTEREST AMT OVER 99999
      * E589 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4630-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-REJECT-CD = SPACES
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN PBKT-INTEREST-AMT < 0
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN PBKT-INTEREST-AMT > 99999.00
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4640-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       4650-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-INTEREST-AMT ROUNDED =
                   PBKM-LINE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4660-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4670-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-TRAN-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4680-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-REGION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN LAST MAINT DATE IS PAST
      * GL -> PE WHEN LAST MAINT DATE IS PAST
      * PE -> RJ WHEN LAST MAINT DATE IS PAST
      * RJ -> VB WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4690-SET-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4700-AGE-NEXT-DUE.
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
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN MATURITY DATE IS PAST
      * PR -> RB WHEN MATURITY DATE IS PAST
      * RB -> LS WHEN MATURITY DATE IS PAST
      * LS -> CF WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4710-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4720-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 5
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4730-ACCUM-PRINTER.
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
      * CARRYFWD YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4740-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-YTD < 50.00
               MOVE 50.00 TO PBKM-CARRYFWD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-YTD > 10000.00
               MOVE 10000.00 TO PBKM-CARRYFWD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4750-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKT-CARRYFWD-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKT-CARRYFWD-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-CARRYFWD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4760-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 6
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4770-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4780-LIMIT-PRINTED.
           IF PBKM-PRINTED-AMT < 25.00
               MOVE 25.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-AMT > 500.00
               MOVE 500.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4790-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4800-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 60
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4810-CHECK-CLOSE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-PBDTE.
           INITIALIZE PBDTE-PARMS
           MOVE PBKM-PRINTED-YTD
             TO PBDTE-PRINTED-YTD
           MOVE PBKM-INTEREST-YTD
             TO PBDTE-INTEREST-YTD
           MOVE PBKM-UNPOSTED-YTD
             TO PBDTE-UNPOSTED-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBDTE-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-AMT
             TO PBDTE-CARRYFWD-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBDTE-CARRYFWD-YTD
           MOVE PBKM-NEXT-DUE-DATE
             TO PBDTE-NEXT-DUE-DATE
           MOVE PBKM-MATURITY-DATE
             TO PBDTE-MATURITY-DATE
           MOVE PBKM-OPEN-DATE
             TO PBDTE-OPEN-DATE
           MOVE PBKM-CLOSE-DATE
             TO PBDTE-CLOSE-DATE
           MOVE PBKM-REGION-CD
             TO PBDTE-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBDTE-SOURCE-CD
           MOVE PBKM-BRANCH-CD
             TO PBDTE-BRANCH-CD
           MOVE PBKM-PRINTER-CD
             TO PBDTE-PRINTER-CD
           MOVE PBKM-REVIEW-FLAG
             TO PBDTE-REVIEW-FLAG
           MOVE PBKM-EXEMPT-FLAG
             TO PBDTE-EXEMPT-FLAG
           MOVE PBKM-SUSPEND-FLAG
             TO PBDTE-SUSPEND-FLAG
           MOVE PBKM-CYCLE-CNT
             TO PBDTE-CYCLE-CNT
           MOVE PBKM-RETRY-CNT
             TO PBDTE-RETRY-CNT
           MOVE PBKM-MARGIN
             TO PBDTE-MARGIN
           MOVE PBKM-FEE-PCT
             TO PBDTE-FEE-PCT
           MOVE PBKM-CUST-ID
             TO PBDTE-CUST-ID
           MOVE PBKM-NAME
             TO PBDTE-NAME
           CALL 'PBDTE' USING PBDTE-PARMS
           EVALUATE PBDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBDTE-PRINTED-YTD
                     TO PBKM-PRINTED-YTD
                   MOVE PBDTE-INTEREST-YTD
                     TO PBKM-INTEREST-YTD
                   MOVE PBDTE-UNPOSTED-YTD
                     TO PBKM-UNPOSTED-YTD
                   MOVE PBDTE-UNPOSTED-AMT
                     TO PBKM-UNPOSTED-AMT
                   MOVE PBDTE-CARRYFWD-AMT
                     TO PBKM-CARRYFWD-AMT
                   MOVE PBDTE-SUSPEND-FLAG
                     TO PBKM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-PBDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE PBSTAT-UNPOSTED-YTD
                     TO PBKM-UNPOSTED-YTD
                   MOVE PBSTAT-LINE-YTD
                     TO PBKM-LINE-YTD
                   MOVE PBSTAT-CARRYFWD-YTD
                     TO PBKM-CARRYFWD-YTD
                   MOVE PBSTAT-UNPOSTED-AMT
                     TO PBKM-UNPOSTED-AMT
                   MOVE PBSTAT-LINE-AMT
                     TO PBKM-LINE-AMT
                   MOVE PBSTAT-NOTICE-SENT-FLAG
                     TO PBKM-NOTICE-SENT-FLAG
                   MOVE PBSTAT-AUTO-RENEW-FLAG
                     TO PBKM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PBSTAT' TO ABND-PARAGRAPH
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
           DISPLAY 'PBKXMIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PBKXMIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
