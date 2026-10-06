      *================================================================*
      * PROGRAM  : MOREXCP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MONEY ORDERS - EXCEPTION PROCESSING. READS THE MOR  *
      *            TRANSACTIONS, UPDATES THE MOR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. MONEY ORDER AND OFFICIAL CHECK  *
      *            ISSUANCE, PAID ITEMS AND ESCHEAT                    *
      * INPUT    : MORMST (KSDS), MORTRN, BANKCTL                      *
      * OUTPUT   : MOROUT, MOREXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MORISK, MOFEE, MODTE    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-03 GAF  ORIGINAL                                       *
      * 1991-12-18 RJT  NEW OUTSTAND REPORT COLUMNS REQ2713            *
      * 1993-07-02 PQR  ESCHEAT TABLE EXPANDED PRB5315                 *
      * 1993-02-22 GAF  PAID FEE SCHEDULE UPDATE CHG8965               *
      * 1995-12-16 GAF  VOIDED EXCEPTIONS TO OPS QUEUE PRB5309         *
      * 1997-04-20 LMB  ESCHEAT CALCULATION CORRECTED INC1436          *
      * 1998-02-04 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-12-22 RJT  PAID EXCEPTIONS TO OPS QUEUE PRB6391           *
      * 1999-06-16 PQR  FEE CALCULATION CORRECTED REQ8508              *
      * 2001-06-19 SKN  OUTSTAND FEE SCHEDULE UPDATE PRB8564           *
      * 2002-09-06 BHS  VOIDED ROUNDING - HALF UP CHG0578              *
      * 2003-12-13 RJT  FACE STATUS CODES ADDED REQ5046                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOREXCP.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "MORMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MORM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "MORTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "MOROUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "MOREXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY MORMST.
       FD  TRAN-FILE.
       COPY MORTRN.
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
      * WS-PAID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000410000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000825000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000041000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000569000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000215500'.
       01  WS-PAID-SCHED-TBL REDEFINES WS-PAID-SCHED-VALUES.
           05  WS-PAID-SCHED-ROW OCCURS 5 TIMES.
               10  WS-PAID-SCHED-LOW       PIC 9(09)V99.
               10  WS-PAID-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOIDED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOIDED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000757000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000505000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000136000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000448500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000285000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000255000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000152000'.
       01  WS-VOIDED-TIER-TBL REDEFINES WS-VOIDED-TIER-VALUES.
           05  WS-VOIDED-TIER-ROW OCCURS 7 TIMES.
               10  WS-VOIDED-TIER-LOW      PIC 9(09)V99.
               10  WS-VOIDED-TIER-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MOREXCP  MONEY ORDERS'.
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
       COPY MORISKP.
       COPY MOFEEP.
       COPY MODTEP.

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
               MOVE 'MORMST' TO ABND-FILE-NAME
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
           MOVE MORT-KEY TO MORM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-AGE-LAST-ACT
               PERFORM 3010-SET-TYPE
               PERFORM 3020-DESCRIBE-REASON
               PERFORM 3030-LIMIT-FEE
               PERFORM 3040-ACCRUE-FEE
               PERFORM 3050-SPLIT-OUTSTAND
               PERFORM 3060-LIMIT-FACE
               PERFORM 3070-DESCRIBE-ITEM-TYPE
               PERFORM 3080-COUNT-ITEM
               PERFORM 3090-ACCUM-CHANNEL
               PERFORM 3100-CHECK-EFF
               PERFORM 3110-EDIT-CUST-ID
               PERFORM 3120-ASSESS-REVERSAL
               PERFORM 3130-ACCUM-CHANNEL
               PERFORM 3140-EDIT-CONTACT
               PERFORM 3150-ASSESS-STOP
               PERFORM 3160-ACCUM-CHANNEL
               PERFORM 3170-SELECT-DISCOUNT
               PERFORM 3180-SET-CYCLE
               PERFORM 3190-CHECK-LAST-MAINT
               PERFORM 3200-PRORATE-PAID
               PERFORM 3210-SPLIT-FACE
               PERFORM 3220-ACCRUE-PAID
               PERFORM 3230-SPLIT-VOIDED
               PERFORM 3240-ACCRUE-FEE
               PERFORM 3250-EDIT-CUST-ID
               PERFORM 3260-EDIT-REF-NO
               PERFORM 3270-ACCRUE-FEE
               PERFORM 3280-FLAG-FACE
               PERFORM 3290-LIMIT-PAID
               PERFORM 3300-DESCRIBE-REASON
               PERFORM 3310-ACCUM-CHANNEL
               PERFORM 3320-AGE-LAST-ACT
               PERFORM 3330-DESCRIBE-CYCLE
               PERFORM 3340-PRORATE-OUTSTAND
               PERFORM 3350-ACCRUE-OUTSTAND
               PERFORM 3360-CHECK-LAST-ACT
               PERFORM 3370-DESCRIBE-TRAN
               PERFORM 3380-CHECK-CLOSE
               PERFORM 3390-FLAG-FEE
               PERFORM 3400-SPLIT-OUTSTAND
               PERFORM 3410-LIMIT-OUTSTAND
               PERFORM 3420-SPLIT-FACE
               PERFORM 3430-SET-TRAN
               PERFORM 3440-AGE-EFFECTIVE
               PERFORM 3450-SELECT-FEE-PCT
               PERFORM 3460-ACCUM-TYPE
               PERFORM 3470-LIMIT-VOIDED
               PERFORM 3480-COUNT-RETRY
               PERFORM 3490-SPLIT-OUTSTAND
               PERFORM 3500-CHECK-LAST-ACT
               PERFORM 3510-ACCRUE-PAID
               PERFORM 3520-DESCRIBE-REASON
               PERFORM 3530-FLAG-FEE
               PERFORM 3540-ACCUM-PAYEE-TYPE
               PERFORM 3550-LIMIT-ITEM
               PERFORM 3560-ACCUM-CHANNEL
               PERFORM 3570-SET-TRAN
               PERFORM 3580-AGE-POST
               PERFORM 3590-AGE-OPEN
               PERFORM 3600-SPLIT-ESCHEAT
               PERFORM 3610-PRORATE-ESCHEAT
               PERFORM 3620-ASSESS-PREFERRED
               PERFORM 3630-PRORATE-ITEM
               PERFORM 3640-FLAG-FACE
               PERFORM 3650-SET-CYCLE
               PERFORM 3660-SELECT-DISCOUNT
               PERFORM 3670-CHECK-EXPIRY
               PERFORM 3680-COUNT-NOTICE
               PERFORM 3690-ACCRUE-FEE
               PERFORM 3700-PRORATE-FACE
               PERFORM 3710-SET-TRAN
               PERFORM 3720-ASSESS-STOP
               PERFORM 3730-LIMIT-PAID
               PERFORM 3740-LIMIT-FEE
               PERFORM 3750-PRORATE-OUTSTAND
               PERFORM 3760-LIMIT-VOIDED
               PERFORM 3770-AGE-CLOSE
               PERFORM 3780-DESCRIBE-PAYEE-TYPE
               PERFORM 3790-AGE-LAST-ACT
               PERFORM 3800-FLAG-ESCHEAT
               PERFORM 3810-LIMIT-VOIDED
               PERFORM 3820-AGE-EXPIRY
               PERFORM 3830-SPLIT-ESCHEAT
               PERFORM 3840-COUNT-PAYMENT
               PERFORM 3850-DESCRIBE-CYCLE
               PERFORM 3860-ACCUM-ITEM-TYPE
               PERFORM 3870-SPLIT-FEE
               PERFORM 3880-ACCUM-PAYEE-TYPE
               PERFORM 3890-SPLIT-VOIDED
               PERFORM 3900-SPLIT-OUTSTAND
               PERFORM 3910-SELECT-DISCOUNT
               PERFORM 3920-EDIT-OPERATOR
               PERFORM 3930-SELECT-DISCOUNT
               PERFORM 3940-SELECT-DISCOUNT
               PERFORM 3950-SET-PAYEE-TYPE
               PERFORM 3960-SET-TRAN
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-SPLIT-FEE
               PERFORM 3990-LIMIT-ESCHEAT
               PERFORM 4000-FLAG-ITEM
               PERFORM 4010-SPLIT-FEE
               PERFORM 4020-EDIT-DESC
               PERFORM 4030-CHECK-NOTICE
               PERFORM 4040-LIMIT-ESCHEAT
               PERFORM 4050-ACCUM-CYCLE
               PERFORM 4060-SPLIT-OUTSTAND
               PERFORM 4070-SET-PAYEE-TYPE
               PERFORM 4080-SPLIT-OUTSTAND
               PERFORM 4090-FLAG-FACE
               PERFORM 4100-LIMIT-ESCHEAT
               PERFORM 4110-AGE-CLOSE
               PERFORM 4120-DESCRIBE-PAYEE-TYPE
               PERFORM 4130-DESCRIBE-CYCLE
               PERFORM 4140-CHECK-LAST-ACT
               PERFORM 4150-SELECT-DISCOUNT
               PERFORM 4160-SET-CHANNEL
               PERFORM 4170-AGE-LAST-ACT
               PERFORM 4180-FLAG-ITEM
               PERFORM 4190-DESCRIBE-CYCLE
               PERFORM 4200-SPLIT-FEE
               PERFORM 4210-ACCUM-ITEM-TYPE
               PERFORM 4220-COUNT-NOTICE
               PERFORM 4230-FLAG-FACE
               PERFORM 4240-EDIT-CUST-ID
               PERFORM 4250-ACCRUE-FEE-PCT
               PERFORM 4260-SELECT-PENALTY-PCT
               PERFORM 4270-COUNT-PAYMENT
               PERFORM 4280-ACCUM-TRAN
               PERFORM 4290-SELECT-DISCOUNT
               PERFORM 4300-FLAG-PAID
               PERFORM 4310-LIMIT-OUTSTAND
               PERFORM 4320-EDIT-CONTACT
               PERFORM 4330-PRORATE-PAID
               PERFORM 4340-ACCRUE-ESCHEAT
               PERFORM 4350-CHECK-OPEN
               PERFORM 4360-EDIT-REJECT
               PERFORM 4370-ACCRUE-ESCHEAT
               PERFORM 4380-ACCUM-TRAN
               PERFORM 4390-FLAG-ITEM
               PERFORM 4400-ASSESS-PREFERRED
               PERFORM 4410-LIMIT-FACE
               PERFORM 4420-ACCUM-TYPE
               PERFORM 4430-DESCRIBE-TRAN
               PERFORM 4440-FLAG-FACE
               PERFORM 4450-COUNT-NOTICE
               PERFORM 4460-SPLIT-OUTSTAND
               PERFORM 4470-SET-TYPE
               PERFORM 4480-AGE-LAST-PAY
               PERFORM 4490-FLAG-OUTSTAND
               PERFORM 4500-FLAG-PAID
               PERFORM 4510-SELECT-DISCOUNT
               PERFORM 4520-LIMIT-ESCHEAT
               PERFORM 4530-DESCRIBE-TYPE
               PERFORM 4540-AGE-OPEN
               PERFORM 4550-DESCRIBE-PAYEE-TYPE
               PERFORM 4560-ACCRUE-OUTSTAND
               PERFORM 4570-DESCRIBE-TYPE
               PERFORM 4580-AGE-OPEN
               PERFORM 4590-SET-PAYEE-TYPE
               PERFORM 4600-EDIT-ADDR
               PERFORM 4610-ACCRUE-VOIDED
               PERFORM 4620-SPLIT-PAID
               PERFORM 4630-EDIT-MEMO
               PERFORM 4640-SELECT-DISCOUNT
               PERFORM 4650-EDIT-CUST-ID
               PERFORM 4660-CHECK-EFFECTIVE
               PERFORM 4670-PRORATE-FEE
               PERFORM 4680-EDIT-REJECT
               PERFORM 4690-EDIT-NAME
               PERFORM 4700-FLAG-VOIDED
               PERFORM 4710-EDIT-REJECT
               PERFORM 4720-FLAG-ITEM
               PERFORM 4730-CHECK-NOTICE
               PERFORM 4740-EDIT-CONTACT
               PERFORM 4750-ACCRUE-FACE
               PERFORM 4760-ASSESS-OVERRIDE
               PERFORM 4770-DESCRIBE-PAYEE-TYPE
               PERFORM 4780-LIMIT-PAID
               PERFORM 4790-CHECK-CLOSE
               PERFORM 4800-DESCRIBE-ITEM-TYPE
               PERFORM 7010-CALL-MORISK
               PERFORM 7000-CALL-MOFEE
               PERFORM 7020-CALL-MODTE
               REWRITE MOR-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'MORMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM MOR-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN CLOSE DATE IS PAST
      * JDX -> LDX WHEN CLOSE DATE IS PAST
      * LDX -> RHT WHEN CLOSE DATE IS PAST
      * RHT -> AGE WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3030-LIMIT-FEE.
           IF MORM-FEE-AMT < 0.00
               MOVE 0.00 TO MORM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FEE-AMT > 1000.00
               MOVE 1000.00 TO MORM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-FEE.
           IF MORM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FEE-YTD * MORM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-VOIDED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE MORM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-FACE.
           IF MORM-FACE-AMT < 10.00
               MOVE 10.00 TO MORM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FACE-AMT > 5000.00
               MOVE 5000.00 TO MORM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-ITEM.
           IF MORM-ITEM-CNT < 9999999
               ADD 1 TO MORM-ITEM-CNT
           END-IF
           IF MORM-ITEM-CNT NOT < 12
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-CHANNEL-CD
                   ADD MORT-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF MORT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E446 CUST ID REQUIRED
      * E519 ESCHEAT AMT NEGATIVE
      * E868 ESCHEAT AMT OVER 50000
      * E234 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN MORT-ESCHEAT-AMT < 0
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN MORT-ESCHEAT-AMT > 50000.00
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN MORM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3120-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   COMPUTE MORM-PAID-AMT ROUNDED = MORM-FEE-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORM-REASON-CD = 'B'
                   MOVE 5.00 TO MORM-PAID-AMT
               WHEN MORM-REASON-CD = 'L'
                   MOVE 7.00 TO MORM-PAID-AMT
               WHEN OTHER
                   COMPUTE MORM-PAID-AMT ROUNDED = MORM-FEE-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORM-PAID-AMT > 0 AND MORM-PAID-AMT < 10.00
               MOVE 10.00 TO MORM-PAID-AMT
           END-IF
           IF MORM-PAID-AMT > 25.00
               MOVE 25.00 TO MORM-PAID-AMT
           END-IF
           ADD MORM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E254 CONTACT REQUIRED
      * E248 ESCHEAT AMT NEGATIVE
      * E229 ESCHEAT AMT OVER 25000
      * E810 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E254' TO WS-REJECT-CODE
               WHEN MORT-ESCHEAT-AMT < 0
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN MORT-ESCHEAT-AMT > 25000.00
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE ESCHEAT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3150-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORT-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORT-ESCHEAT-AMT
                       * MORM-FEE-PCT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 7.00 TO MORT-OUTSTAND-AMT
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 20.00 TO MORT-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORT-ESCHEAT-AMT
                       * MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORT-OUTSTAND-AMT > 0 AND MORT-OUTSTAND-AMT < 10.00
               MOVE 10.00 TO MORT-OUTSTAND-AMT
           END-IF
           IF MORT-OUTSTAND-AMT > 250.00
               MOVE 250.00 TO MORT-OUTSTAND-AMT
           END-IF
           ADD MORT-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-PAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-PAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE VOIDED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-VOIDED-YTD NOT < WS-VOIDED-TIER-LOW(WS-TX)
                   MOVE WS-VOIDED-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN OPEN DATE IS PAST
      * GEJ -> JBE WHEN OPEN DATE IS PAST
      * JBE -> DGU WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3200-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-PAID-YTD ROUNDED =
                   MORT-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-PAID-YTD
           END-IF
           ADD MORM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = MORM-FACE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORM-FACE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-PAID.
           IF MORM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-PAID-YTD * MORM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MORM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOIDED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-AMT - (WS-WORK-AMT * 2)
           COMPUTE MORM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-FEE.
           IF MORT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-FEE-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E633 CUST ID REQUIRED
      * E021 OUTSTAND AMT NEGATIVE
      * E212 OUTSTAND AMT OVER 10000
      * E847 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-AMT < 0
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-AMT > 10000.00
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN MORM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E245 REF NO REQUIRED
      * E219 VOIDED AMT NEGATIVE
      * E433 VOIDED AMT OVER 5000
      * E918 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-REF-NO = SPACES
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN MORM-VOIDED-AMT < 0
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN MORM-VOIDED-AMT > 5000.00
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN MORM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-FEE.
           IF MORM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FEE-AMT * MORM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MORT-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3280-FLAG-FACE.
           IF MORM-FACE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3290-LIMIT-PAID.
           IF MORM-PAID-YTD < 0.00
               MOVE 0.00 TO MORM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-PAID-YTD > 5000.00
               MOVE 5000.00 TO MORM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3340-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-OUTSTAND-AMT ROUNDED =
                   MORT-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORT-OUTSTAND-AMT
           END-IF
           ADD MORT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3350-ACCRUE-OUTSTAND.
           IF MORT-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-OUTSTAND-AMT * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ES'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MORM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3390-FLAG-FEE.
           IF MORM-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORT-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3410-LIMIT-OUTSTAND.
           IF MORT-OUTSTAND-AMT < 25.00
               MOVE 25.00 TO MORT-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-OUTSTAND-AMT > 2500.00
               MOVE 2500.00 TO MORT-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = MORM-FACE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORM-FACE-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN OPEN DATE IS PAST
      * PD -> VD WHEN OPEN DATE IS PAST
      * VD -> SP WHEN OPEN DATE IS PAST
      * SP -> ES WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MORM-FEE-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOIDED AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3470-LIMIT-VOIDED.
           IF MORM-VOIDED-AMT < 50.00
               MOVE 50.00 TO MORM-VOIDED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-VOIDED-AMT > 10000.00
               MOVE 10000.00 TO MORM-VOIDED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 10
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-AMT - (WS-WORK-AMT * 3)
           COMPUTE MORM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-PAID.
           IF MORM-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-PAID-AMT * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3530-FLAG-FEE.
           IF MORT-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-PAYEE-TYPE-CD
                   ADD MORM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST ACT DATE IS PAST
      * PD -> VD WHEN LAST ACT DATE IS PAST
      * VD -> SP WHEN LAST ACT DATE IS PAST
      * SP -> ES WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0 AND MORT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MORT-ESCHEAT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORT-ESCHEAT-AMT - (WS-WORK-AMT * 2)
           COMPUTE MORT-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ESCHEAT AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-ESCHEAT-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-ESCHEAT-AMT
           END-IF
           ADD MORM-ESCHEAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE VOIDED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3620-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORT-FEE-AMT ROUNDED = MORM-VOIDED-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-PAYEE-TYPE-CD = 'CJ'
                   MOVE 12.00 TO MORT-FEE-AMT
               WHEN MORM-PAYEE-TYPE-CD = 'FY'
                   MOVE 20.00 TO MORT-FEE-AMT
               WHEN OTHER
                   COMPUTE MORT-FEE-AMT ROUNDED = MORM-VOIDED-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORT-FEE-AMT > 0 AND MORT-FEE-AMT < 1.00
               MOVE 1.00 TO MORT-FEE-AMT
           END-IF
           IF MORT-FEE-AMT > 25.00
               MOVE 25.00 TO MORT-FEE-AMT
           END-IF
           ADD MORT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FACE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3640-FLAG-FACE.
           IF MORT-FACE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORT-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN NOTICE DATE IS PAST
      * GEJ -> JBE WHEN NOTICE DATE IS PAST
      * JBE -> DGU WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MORT-FEE-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-FEE.
           IF MORT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-FEE-AMT * MORM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-VOIDED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST MAINT DATE IS PAST
      * PD -> VD WHEN LAST MAINT DATE IS PAST
      * VD -> SP WHEN LAST MAINT DATE IS PAST
      * SP -> ES WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3720-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORT-ESCHEAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = MORM-OUTSTAND-YTD
                       * MORM-PENALTY-PCT / 200
               WHEN MORM-REASON-CD = 'B'
                   MOVE 3.00 TO MORT-ESCHEAT-AMT
               WHEN MORM-REASON-CD = 'L'
                   MOVE 15.00 TO MORT-ESCHEAT-AMT
               WHEN OTHER
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = MORM-OUTSTAND-YTD
                       * MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORT-ESCHEAT-AMT > 0 AND MORT-ESCHEAT-AMT < 2.00
               MOVE 2.00 TO MORT-ESCHEAT-AMT
           END-IF
           IF MORT-ESCHEAT-AMT > 50.00
               MOVE 50.00 TO MORT-ESCHEAT-AMT
           END-IF
           ADD MORT-ESCHEAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-PAID.
           IF MORM-PAID-AMT < 25.00
               MOVE 25.00 TO MORM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-PAID-AMT > 1000.00
               MOVE 1000.00 TO MORM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-FEE.
           IF MORT-FEE-AMT < 100.00
               MOVE 100.00 TO MORT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FEE-AMT > 1000.00
               MOVE 1000.00 TO MORT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3750-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-OUTSTAND-YTD ROUNDED =
                   MORT-OUTSTAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-OUTSTAND-YTD
           END-IF
           ADD MORM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VOIDED YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3760-LIMIT-VOIDED.
           IF MORM-VOIDED-YTD < 25.00
               MOVE 25.00 TO MORM-VOIDED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-VOIDED-YTD > 2500.00
               MOVE 2500.00 TO MORM-VOIDED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ESCHEAT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-ESCHEAT.
           IF MORM-ESCHEAT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VOIDED YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3810-LIMIT-VOIDED.
           IF MORM-VOIDED-YTD < 100.00
               MOVE 100.00 TO MORM-VOIDED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-VOIDED-YTD > 500.00
               MOVE 500.00 TO MORM-VOIDED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MORT-ESCHEAT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORT-ESCHEAT-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-ITEM-TYPE-CD
                   ADD MORT-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-PAYEE-TYPE-CD
                   ADD MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOIDED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORM-FACE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORT-OUTSTAND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORT-OUTSTAND-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-PAID-YTD NOT < WS-VOIDED-TIER-LOW(WS-TX)
                   MOVE WS-VOIDED-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E597 OPERATOR REQUIRED
      * E177 PAID YTD NEGATIVE
      * E626 PAID YTD OVER 5000
      * E019 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-OPERATOR = SPACES
                   MOVE 'E597' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD < 0
                   MOVE 'E177' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD > 5000.00
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN MORM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-VOIDED-TIER-LOW(WS-TX)
                   MOVE WS-VOIDED-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE ESCHEAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-ESCHEAT-AMT NOT < WS-VOIDED-TIER-LOW(WS-TX)
                   MOVE WS-VOIDED-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN LAST PAY DATE IS PAST
      * FY -> HX WHEN LAST PAY DATE IS PAST
      * HX -> UV WHEN LAST PAY DATE IS PAST
      * UV -> VV WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST STMT DATE IS PAST
      * PD -> VD WHEN LAST STMT DATE IS PAST
      * VD -> SP WHEN LAST STMT DATE IS PAST
      * SP -> ES WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-FACE-AMT NOT < WS-VOIDED-TIER-LOW(WS-TX)
                   MOVE WS-VOIDED-TIER-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE MORM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3990-LIMIT-ESCHEAT.
           IF MORT-ESCHEAT-AMT < 50.00
               MOVE 50.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-ESCHEAT-AMT > 2500.00
               MOVE 2500.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4000-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE MORM-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E202 DESC REQUIRED
      * E532 ESCHEAT YTD NEGATIVE
      * E517 ESCHEAT YTD OVER 25000
      * E254 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-YTD < 0
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-YTD > 25000.00
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN MORM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E254' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MORM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4040-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-AMT < 25.00
               MOVE 25.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-AMT > 10000.00
               MOVE 10000.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-CYCLE-CD
                   ADD MORM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN LAST MAINT DATE IS PAST
      * FY -> HX WHEN LAST MAINT DATE IS PAST
      * HX -> UV WHEN LAST MAINT DATE IS PAST
      * UV -> VV WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MORM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4090-FLAG-FACE.
           IF MORM-FACE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-AMT < 100.00
               MOVE 100.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-AMT > 10000.00
               MOVE 10000.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MORT-FEE-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-CHANNEL.
           EVALUATE MORT-CHANNEL-CD
               WHEN 'BRN'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4180-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MORM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOIDED AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-ITEM-TYPE-CD
                   ADD MORM-VOIDED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-VOIDED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO MORM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4230-FLAG-FACE.
           IF MORM-FACE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E144 CUST ID REQUIRED
      * E871 OUTSTAND AMT NEGATIVE
      * E647 OUTSTAND AMT OVER 1000000
      * E738 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN MORT-OUTSTAND-AMT < 0
                   MOVE 'E871' TO WS-REJECT-CODE
               WHEN MORT-OUTSTAND-AMT > 1000000.00
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN MORM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE ESCHEAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MORT-ESCHEAT-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4270-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO MORM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-TRAN-CD
                   ADD MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE ESCHEAT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MORM-ESCHEAT-YTD NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4300-FLAG-PAID.
           IF MORM-PAID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4310-LIMIT-OUTSTAND.
           IF MORT-OUTSTAND-AMT < 10.00
               MOVE 10.00 TO MORT-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-OUTSTAND-AMT > 2500.00
               MOVE 2500.00 TO MORT-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E164 CONTACT REQUIRED
      * E050 PAID YTD NEGATIVE
      * E408 PAID YTD OVER 99999
      * E437 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4320-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD < 0
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD > 99999.00
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       4330-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-PAID-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-PAID-AMT
           END-IF
           ADD MORM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4340-ACCRUE-ESCHEAT.
           IF MORM-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-ESCHEAT-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-ESCHEAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MORM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E601 REJECT CD REQUIRED
      * E038 ESCHEAT AMT NEGATIVE
      * E354 ESCHEAT AMT OVER 10000
      * E796 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4360-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT < 0
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT > 10000.00
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN MORM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4370-ACCRUE-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4380-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-TRAN-CD
                   ADD MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4390-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4400-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORT-FEE-AMT ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 3.00 TO MORT-FEE-AMT
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 12.00 TO MORT-FEE-AMT
               WHEN OTHER
                   COMPUTE MORT-FEE-AMT ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORT-FEE-AMT > 0 AND MORT-FEE-AMT < 1.00
               MOVE 1.00 TO MORT-FEE-AMT
           END-IF
           IF MORT-FEE-AMT > 75.00
               MOVE 75.00 TO MORT-FEE-AMT
           END-IF
           ADD MORT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4410-LIMIT-FACE.
           IF MORM-FACE-AMT < 0.00
               MOVE 0.00 TO MORM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FACE-AMT > 5000.00
               MOVE 5000.00 TO MORM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4420-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4430-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ES'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4440-FLAG-FACE.
           IF MORM-FACE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4450-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-AMT - (WS-WORK-AMT * 3)
           COMPUTE MORM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN POST DATE IS PAST
      * JDX -> LDX WHEN POST DATE IS PAST
      * LDX -> RHT WHEN POST DATE IS PAST
      * RHT -> AGE WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4470-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4480-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4490-FLAG-OUTSTAND.
           IF MORM-OUTSTAND-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4500-FLAG-PAID.
           IF MORM-PAID-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4510-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MORM-FACE-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4520-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-AMT < 100.00
               MOVE 100.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-AMT > 10000.00
               MOVE 10000.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4530-DESCRIBE-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JDX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LDX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RHT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4540-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4550-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4560-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4570-DESCRIBE-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JDX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LDX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RHT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4580-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN POST DATE IS PAST
      * FY -> HX WHEN POST DATE IS PAST
      * HX -> UV WHEN POST DATE IS PAST
      * UV -> VV WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4590-SET-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E888 ADDR REQUIRED
      * E937 FACE AMT NEGATIVE
      * E124 FACE AMT OVER 250000
      * E276 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4600-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-ADDR = SPACES
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT < 0
                   MOVE 'E937' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT > 250000.00
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN MORM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOIDED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4610-ACCRUE-VOIDED.
           IF MORM-VOIDED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-VOIDED-YTD * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-VOIDED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4620-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = MORM-PAID-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORM-PAID-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORM-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E375 MEMO REQUIRED
      * E446 FACE AMT NEGATIVE
      * E355 FACE AMT OVER 50000
      * E104 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4630-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-MEMO = SPACES
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT < 0
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT > 50000.00
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN MORM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4640-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-FACE-AMT NOT < WS-VOIDED-TIER-LOW(WS-TX)
                   MOVE WS-VOIDED-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E991 CUST ID REQUIRED
      * E891 OUTSTAND AMT NEGATIVE
      * E233 OUTSTAND AMT OVER 10000
      * E608 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4650-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E991' TO WS-REJECT-CODE
               WHEN MORT-OUTSTAND-AMT < 0
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN MORT-OUTSTAND-AMT > 10000.00
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN MORM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4660-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF MORM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MORM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4670-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-FEE-YTD ROUNDED =
                   MORM-ESCHEAT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-FEE-YTD
           END-IF
           ADD MORM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E410 REJECT CD REQUIRED
      * E489 PAID YTD NEGATIVE
      * E396 PAID YTD OVER 250000
      * E222 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4680-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD < 0
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD > 250000.00
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN MORM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E505 NAME REQUIRED
      * E992 PAID YTD NEGATIVE
      * E553 PAID YTD OVER 5000
      * E044 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4690-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-NAME = SPACES
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD < 0
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD > 5000.00
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN MORM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOIDED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4700-FLAG-VOIDED.
           IF MORM-VOIDED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-VOIDED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E680 REJECT CD REQUIRED
      * E897 FACE AMT NEGATIVE
      * E981 FACE AMT OVER 50000
      * E112 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4710-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT < 0
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT > 50000.00
                   MOVE 'E981' TO WS-REJECT-CODE
               WHEN MORM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4720-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4730-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MORM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E191 CONTACT REQUIRED
      * E501 ESCHEAT AMT NEGATIVE
      * E336 ESCHEAT AMT OVER 99999
      * E173 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4740-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT < 0
                   MOVE 'E501' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT > 99999.00
                   MOVE 'E336' TO WS-REJECT-CODE
               WHEN MORM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4750-ACCRUE-FACE.
           IF MORT-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-FACE-AMT * MORM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MORM-ESCHEAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4760-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MORT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   COMPUTE MORT-FEE-AMT ROUNDED = WS-WORK-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 12.00 TO MORT-FEE-AMT
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 7.00 TO MORT-FEE-AMT
               WHEN OTHER
                   COMPUTE MORT-FEE-AMT ROUNDED = WS-WORK-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORT-FEE-AMT > 0 AND MORT-FEE-AMT < 2.00
               MOVE 2.00 TO MORT-FEE-AMT
           END-IF
           IF MORT-FEE-AMT > 150.00
               MOVE 150.00 TO MORT-FEE-AMT
           END-IF
           ADD MORT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4770-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4780-LIMIT-PAID.
           IF MORM-PAID-AMT < 10.00
               MOVE 10.00 TO MORM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-PAID-AMT > 10000.00
               MOVE 10000.00 TO MORM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4790-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MORM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4800-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MOR FIELDS TO MORISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-MORISK.
           INITIALIZE MORISK-PARMS
           MOVE MORM-ESCHEAT-YTD
             TO MORISK-ESCHEAT-YTD
           MOVE MORM-OUTSTAND-YTD
             TO MORISK-OUTSTAND-YTD
           MOVE MORM-PAID-AMT
             TO MORISK-PAID-AMT
           MOVE MORM-FEE-AMT
             TO MORISK-FEE-AMT
           MOVE MORM-FEE-YTD
             TO MORISK-FEE-YTD
           MOVE MORM-OUTSTAND-AMT
             TO MORISK-OUTSTAND-AMT
           MOVE MORM-NOTICE-DATE
             TO MORISK-NOTICE-DATE
           MOVE MORM-OPEN-DATE
             TO MORISK-OPEN-DATE
           MOVE MORM-LAST-ACT-DATE
             TO MORISK-LAST-ACT-DATE
           MOVE MORM-EXPIRY-DATE
             TO MORISK-EXPIRY-DATE
           MOVE MORM-REASON-CD
             TO MORISK-REASON-CD
           MOVE MORM-CYCLE-CD
             TO MORISK-CYCLE-CD
           MOVE MORM-PAYEE-TYPE-CD
             TO MORISK-PAYEE-TYPE-CD
           MOVE MORM-CHANNEL-CD
             TO MORISK-CHANNEL-CD
           MOVE MORM-PREFERRED-FLAG
             TO MORISK-PREFERRED-FLAG
           MOVE MORM-STOP-FLAG
             TO MORISK-STOP-FLAG
           MOVE MORM-AUTO-RENEW-FLAG
             TO MORISK-AUTO-RENEW-FLAG
           MOVE MORM-ITEM-CNT
             TO MORISK-ITEM-CNT
           MOVE MORM-RETRY-CNT
             TO MORISK-RETRY-CNT
           MOVE MORM-PENALTY-PCT
             TO MORISK-PENALTY-PCT
           MOVE MORM-FEE-PCT
             TO MORISK-FEE-PCT
           MOVE MORM-ADDR
             TO MORISK-ADDR
           MOVE MORM-DESC
             TO MORISK-DESC
           CALL 'MORISK' USING MORISK-PARMS
           EVALUATE MORISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MORISK-ESCHEAT-YTD
                     TO MORM-ESCHEAT-YTD
                   MOVE MORISK-OUTSTAND-YTD
                     TO MORM-OUTSTAND-YTD
                   MOVE MORISK-PAID-AMT
                     TO MORM-PAID-AMT
                   MOVE MORISK-FEE-AMT
                     TO MORM-FEE-AMT
                   MOVE MORISK-FEE-YTD
                     TO MORM-FEE-YTD
                   MOVE MORISK-PREFERRED-FLAG
                     TO MORM-PREFERRED-FLAG
                   MOVE MORISK-STOP-FLAG
                     TO MORM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-MORISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MOR FIELDS TO MOFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-MOFEE.
           INITIALIZE MOFEE-PARMS
           MOVE MORM-OUTSTAND-AMT
             TO MOFEE-OUTSTAND-AMT
           MOVE MORM-ESCHEAT-AMT
             TO MOFEE-ESCHEAT-AMT
           MOVE MORM-PAID-YTD
             TO MOFEE-PAID-YTD
           MOVE MORM-PAID-AMT
             TO MOFEE-PAID-AMT
           MOVE MORM-FACE-AMT
             TO MOFEE-FACE-AMT
           MOVE MORM-FEE-AMT
             TO MOFEE-FEE-AMT
           MOVE MORM-LAST-STMT-DATE
             TO MOFEE-LAST-STMT-DATE
           MOVE MORM-LAST-PAY-DATE
             TO MOFEE-LAST-PAY-DATE
           MOVE MORM-LAST-ACT-DATE
             TO MOFEE-LAST-ACT-DATE
           MOVE MORM-EFFECTIVE-DATE
             TO MOFEE-EFFECTIVE-DATE
           MOVE MORM-PAYEE-TYPE-CD
             TO MOFEE-PAYEE-TYPE-CD
           MOVE MORM-CHANNEL-CD
             TO MOFEE-CHANNEL-CD
           MOVE MORM-REASON-CD
             TO MOFEE-REASON-CD
           MOVE MORM-ITEM-TYPE-CD
             TO MOFEE-ITEM-TYPE-CD
           MOVE MORM-OVERRIDE-FLAG
             TO MOFEE-OVERRIDE-FLAG
           MOVE MORM-STOP-FLAG
             TO MOFEE-STOP-FLAG
           MOVE MORM-PREFERRED-FLAG
             TO MOFEE-PREFERRED-FLAG
           MOVE MORM-ITEM-CNT
             TO MOFEE-ITEM-CNT
           MOVE MORM-RETRY-CNT
             TO MOFEE-RETRY-CNT
           MOVE MORM-DISCOUNT
             TO MOFEE-DISCOUNT
           MOVE MORM-FEE-PCT
             TO MOFEE-FEE-PCT
           MOVE MORM-REF-NO
             TO MOFEE-REF-NO
           MOVE MORM-NAME
             TO MOFEE-NAME
           CALL 'MOFEE' USING MOFEE-PARMS
           EVALUATE MOFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MOFEE-PAID-YTD
                     TO MORM-PAID-YTD
                   MOVE MOFEE-PAID-AMT
                     TO MORM-PAID-AMT
                   MOVE MOFEE-FEE-AMT
                     TO MORM-FEE-AMT
                   MOVE MOFEE-PREFERRED-FLAG
                     TO MORM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-MOFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MOR FIELDS TO MODTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-MODTE.
           INITIALIZE MODTE-PARMS
           MOVE MORM-FEE-YTD
             TO MODTE-FEE-YTD
           MOVE MORM-FEE-AMT
             TO MODTE-FEE-AMT
           MOVE MORM-PAID-AMT
             TO MODTE-PAID-AMT
           MOVE MORM-VOIDED-AMT
             TO MODTE-VOIDED-AMT
           MOVE MORM-ESCHEAT-AMT
             TO MODTE-ESCHEAT-AMT
           MOVE MORM-FACE-AMT
             TO MODTE-FACE-AMT
           MOVE MORM-LAST-MAINT-DATE
             TO MODTE-LAST-MAINT-DATE
           MOVE MORM-EXPIRY-DATE
             TO MODTE-EXPIRY-DATE
           MOVE MORM-CLOSE-DATE
             TO MODTE-CLOSE-DATE
           MOVE MORM-OPEN-DATE
             TO MODTE-OPEN-DATE
           MOVE MORM-ITEM-TYPE-CD
             TO MODTE-ITEM-TYPE-CD
           MOVE MORM-REASON-CD
             TO MODTE-REASON-CD
           MOVE MORM-TYPE-CD
             TO MODTE-TYPE-CD
           MOVE MORM-CYCLE-CD
             TO MODTE-CYCLE-CD
           MOVE MORM-STOP-FLAG
             TO MODTE-STOP-FLAG
           MOVE MORM-AUTO-RENEW-FLAG
             TO MODTE-AUTO-RENEW-FLAG
           MOVE MORM-EXEMPT-FLAG
             TO MODTE-EXEMPT-FLAG
           MOVE MORM-PAYMENT-CNT
             TO MODTE-PAYMENT-CNT
           MOVE MORM-ITEM-CNT
             TO MODTE-ITEM-CNT
           MOVE MORM-FEE-PCT
             TO MODTE-FEE-PCT
           MOVE MORM-DISCOUNT
             TO MODTE-DISCOUNT
           MOVE MORM-NAME
             TO MODTE-NAME
           MOVE MORM-REF-NO
             TO MODTE-REF-NO
           CALL 'MODTE' USING MODTE-PARMS
           EVALUATE MODTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MODTE-PAID-AMT
                     TO MORM-PAID-AMT
                   MOVE MODTE-VOIDED-AMT
                     TO MORM-VOIDED-AMT
                   MOVE MODTE-FACE-AMT
                     TO MORM-FACE-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-MODTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE MORT-KEY TO RD-KEY
           MOVE MORT-TRAN-CD TO RD-CODE
           MOVE MORT-FACE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE MORT-KEY TO RD-KEY
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
           DISPLAY 'MOREXCP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MOREXCP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
