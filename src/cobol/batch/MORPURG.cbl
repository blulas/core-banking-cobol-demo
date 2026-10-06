      *================================================================*
      * PROGRAM  : MORPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MONEY ORDERS - PURGE AND ARCHIVE. READS THE MOR     *
      *            TRANSACTIONS, UPDATES THE MOR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. MONEY ORDER AND OFFICIAL CHECK  *
      *            ISSUANCE, PAID ITEMS AND ESCHEAT                    *
      * INPUT    : MORMST (KSDS), MORTRN, BANKCTL                      *
      * OUTPUT   : MOROUT, MORPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MORISK, MODTE           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-02 MKL  ORIGINAL                                       *
      * 1991-01-23 RJT  FEE ROUNDING - HALF UP CHG9931                 *
      * 1992-02-17 MKL  OUTSTAND STATUS CODES ADDED INC4864            *
      * 1995-10-15 PQR  NEW OUTSTAND REPORT COLUMNS REQ8658            *
      * 1995-08-02 MKL  PAID CALCULATION CORRECTED CHG3094             *
      * 1996-12-20 RJT  VOIDED EXCEPTIONS TO OPS QUEUE INC4445         *
      * 1996-10-14 TWH  OUTSTAND RERUN PROTECTION PRB1438              *
      * 1997-11-18 GAF  FACE RERUN PROTECTION REQ6199                  *
      * 1997-10-28 DPS  NEW ESCHEAT REPORT COLUMNS REQ5879             *
      * 2002-01-08 JLM  VOIDED FEE SCHEDULE UPDATE INC4009             *
      * 2003-07-11 RJT  ESCHEAT RERUN PROTECTION CHG1216               *
      * 2006-04-08 JLM  ESCHEAT EXCEPTIONS TO OPS QUEUE INC2227        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORPURG.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "MORPURPT"
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
                                           VALUE 12.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
                                           VALUE '00000000000000536000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000054000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000489500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000042000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000601500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000736000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001850000000247500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002100000000128000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002200000000323500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007200000000481000'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 10 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000187000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000182000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000356000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000324000'.
       01  WS-PAID-SCHED-TBL REDEFINES WS-PAID-SCHED-VALUES.
           05  WS-PAID-SCHED-ROW OCCURS 4 TIMES.
               10  WS-PAID-SCHED-LOW       PIC 9(09)V99.
               10  WS-PAID-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-OUTSTAND-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000059000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000688000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000112500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000080000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000050000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000681000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000168500'.
       01  WS-OUTSTAND-BAND-TBL REDEFINES WS-OUTSTAND-BAND-VALUES.
           05  WS-OUTSTAND-BAND-ROW OCCURS 7 TIMES.
               10  WS-OUTSTAND-BAND-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FACE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000319000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000351500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000345000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000549000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000409000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000223000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025600000000874500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035600000000022000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035850000000079000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035950000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00045950000000042000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00050950000000073000'.
       01  WS-FACE-SCHED-TBL REDEFINES WS-FACE-SCHED-VALUES.
           05  WS-FACE-SCHED-ROW OCCURS 12 TIMES.
               10  WS-FACE-SCHED-LOW       PIC 9(09)V99.
               10  WS-FACE-SCHED-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MORPURG  MONEY ORDERS'.
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
               PERFORM 3000-ACCUM-PAYEE-TYPE
               PERFORM 3010-ACCRUE-VOIDED
               PERFORM 3020-CHECK-OPEN
               PERFORM 3030-AGE-LAST-PAY
               PERFORM 3040-CHECK-NOTICE
               PERFORM 3050-SET-TRAN
               PERFORM 3060-FLAG-ITEM
               PERFORM 3070-EDIT-CUST-ID
               PERFORM 3080-ACCRUE-PAID
               PERFORM 3090-EDIT-CUST-ID
               PERFORM 3100-COUNT-PAYMENT
               PERFORM 3110-FLAG-PAID
               PERFORM 3120-SELECT-FEE-PCT
               PERFORM 3130-SET-CYCLE
               PERFORM 3140-SELECT-FEE-PCT
               PERFORM 3150-SPLIT-VOIDED
               PERFORM 3160-PRORATE-OUTSTAND
               PERFORM 3170-CHECK-LAST-STMT
               PERFORM 3180-SET-PAYEE-TYPE
               PERFORM 3190-DESCRIBE-PAYEE-TYPE
               PERFORM 3200-SPLIT-VOIDED
               PERFORM 3210-SPLIT-FACE
               PERFORM 3220-LIMIT-FEE
               PERFORM 3230-PRORATE-PAID
               PERFORM 3240-EDIT-DESC
               PERFORM 3250-AGE-LAST-PAY
               PERFORM 3260-COUNT-RETRY
               PERFORM 3270-ASSESS-EXEMPT
               PERFORM 3280-DESCRIBE-ITEM-TYPE
               PERFORM 3290-AGE-CLOSE
               PERFORM 3300-ASSESS-STOP
               PERFORM 3310-ACCRUE-FEE
               PERFORM 3320-LIMIT-ESCHEAT
               PERFORM 3330-ASSESS-PREFERRED
               PERFORM 3340-ACCRUE-VOIDED
               PERFORM 3350-EDIT-REJECT
               PERFORM 3360-SET-CHANNEL
               PERFORM 3370-LIMIT-ESCHEAT
               PERFORM 3380-AGE-LAST-PAY
               PERFORM 3390-AGE-LAST-MAINT
               PERFORM 3400-ACCRUE-FACE
               PERFORM 3410-FLAG-FACE
               PERFORM 3420-SET-CHANNEL
               PERFORM 3430-ACCRUE-OUTSTAND
               PERFORM 3440-CHECK-LAST-MAINT
               PERFORM 3450-CHECK-EXPIRY
               PERFORM 3460-ACCRUE-FACE
               PERFORM 3470-PRORATE-VOIDED
               PERFORM 3480-SET-CHANNEL
               PERFORM 3490-LIMIT-VOIDED
               PERFORM 3500-LIMIT-ESCHEAT
               PERFORM 3510-EDIT-REJECT
               PERFORM 3520-SELECT-DISCOUNT
               PERFORM 3530-SPLIT-FEE
               PERFORM 3540-FLAG-ESCHEAT
               PERFORM 3550-COUNT-PAYMENT
               PERFORM 3560-EDIT-REF-NO
               PERFORM 3570-FLAG-ITEM
               PERFORM 3580-DESCRIBE-ITEM-TYPE
               PERFORM 3590-DESCRIBE-TRAN
               PERFORM 3600-SPLIT-FEE
               PERFORM 3610-ASSESS-PREFERRED
               PERFORM 3620-LIMIT-PAID
               PERFORM 3630-CHECK-OPEN
               PERFORM 3640-AGE-LAST-PAY
               PERFORM 3650-LIMIT-PAID
               PERFORM 3660-COUNT-SEQ
               PERFORM 3670-COUNT-ITEM
               PERFORM 3680-ACCRUE-OUTSTAND
               PERFORM 3690-AGE-LAST-MAINT
               PERFORM 3700-AGE-LAST-STMT
               PERFORM 3710-PRORATE-VOIDED
               PERFORM 3720-SET-TYPE
               PERFORM 3730-ACCUM-CHANNEL
               PERFORM 3740-COUNT-RETRY
               PERFORM 3750-FLAG-PAID
               PERFORM 3760-AGE-LAST-STMT
               PERFORM 3770-EDIT-MEMO
               PERFORM 3780-EDIT-DESC
               PERFORM 3790-CHECK-EFFECTIVE
               PERFORM 3800-LIMIT-OUTSTAND
               PERFORM 3810-DESCRIBE-TRAN
               PERFORM 3820-PRORATE-FEE
               PERFORM 3830-FLAG-ESCHEAT
               PERFORM 3840-COUNT-ITEM
               PERFORM 3850-CHECK-POST
               PERFORM 3860-AGE-LAST-MAINT
               PERFORM 3870-SET-CYCLE
               PERFORM 3880-DESCRIBE-PAYEE-TYPE
               PERFORM 3890-FLAG-OUTSTAND
               PERFORM 3900-ACCRUE-FEE-PCT
               PERFORM 3910-ACCRUE-PAID
               PERFORM 3920-PRORATE-ESCHEAT
               PERFORM 3930-SPLIT-OUTSTAND
               PERFORM 3940-SET-CHANNEL
               PERFORM 3950-AGE-NOTICE
               PERFORM 3960-SPLIT-ESCHEAT
               PERFORM 3970-ACCUM-TYPE
               PERFORM 3980-LIMIT-FEE
               PERFORM 3990-SET-REASON
               PERFORM 4000-AGE-LAST-ACT
               PERFORM 4010-EDIT-CONTACT
               PERFORM 4020-FLAG-ITEM
               PERFORM 4030-LIMIT-FACE
               PERFORM 4040-ACCRUE-DISCOUNT
               PERFORM 4050-ACCUM-TYPE
               PERFORM 4060-SELECT-PENALTY-PCT
               PERFORM 4070-AGE-EXPIRY
               PERFORM 4080-SPLIT-FEE
               PERFORM 4090-COUNT-ITEM
               PERFORM 4100-DESCRIBE-TRAN
               PERFORM 4110-DESCRIBE-CHANNEL
               PERFORM 4120-EDIT-DESC
               PERFORM 7010-CALL-MORISK
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
      * ACCUMULATE VOIDED YTD BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-PAYEE-TYPE-CD
                   ADD MORM-VOIDED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-VOIDED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOIDED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-VOIDED.
           IF MORM-VOIDED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-VOIDED-AMT * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-OPEN.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MORT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-LAST-PAY.
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
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-NOTICE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST MAINT DATE IS PAST
      * PD -> VD WHEN LAST MAINT DATE IS PAST
      * VD -> SP WHEN LAST MAINT DATE IS PAST
      * SP -> ES WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-TRAN.
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
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3060-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E319 CUST ID REQUIRED
      * E972 ESCHEAT AMT NEGATIVE
      * E325 ESCHEAT AMT OVER 1000000
      * E605 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT < 0
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT > 1000000.00
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-PAID.
           IF MORM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-PAID-YTD * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E063 CUST ID REQUIRED
      * E539 FACE AMT NEGATIVE
      * E220 FACE AMT OVER 250000
      * E044 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT < 0
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT > 250000.00
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO MORM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-PAID.
           IF MORM-PAID-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-FEE-AMT NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN LAST ACT DATE IS PAST
      * GEJ -> JBE WHEN LAST ACT DATE IS PAST
      * JBE -> DGU WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE VOIDED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF MORM-VOIDED-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-VOIDED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-OUTSTAND-YTD ROUNDED =
                   MORM-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-OUTSTAND-YTD
           END-IF
           ADD MORM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN LAST STMT DATE IS PAST
      * FY -> HX WHEN LAST STMT DATE IS PAST
      * HX -> UV WHEN LAST STMT DATE IS PAST
      * UV -> VV WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOIDED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-ESCHEAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = MORM-FACE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MORM-FACE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MORM-ESCHEAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-FEE.
           IF MORM-FEE-AMT < 0.00
               MOVE 0.00 TO MORM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FEE-AMT > 5000.00
               MOVE 5000.00 TO MORM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3230-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-PAID-YTD ROUNDED =
                   MORT-ESCHEAT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-PAID-YTD
           END-IF
           ADD MORM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E800 DESC REQUIRED
      * E659 PAID YTD NEGATIVE
      * E852 PAID YTD OVER 1000000
      * E442 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD < 0
                   MOVE 'E659' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD > 1000000.00
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN MORM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-LAST-PAY.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 6
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOIDED FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3270-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORM-VOIDED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORM-VOIDED-AMT ROUNDED = MORT-OUTSTAND-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORT-TRAN-CD = 'IS'
                   MOVE 5.00 TO MORM-VOIDED-AMT
               WHEN MORT-TRAN-CD = 'PD'
                   MOVE 7.00 TO MORM-VOIDED-AMT
               WHEN OTHER
                   COMPUTE MORM-VOIDED-AMT ROUNDED = MORT-OUTSTAND-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-VOIDED-AMT > 0 AND MORM-VOIDED-AMT < 2.00
               MOVE 2.00 TO MORM-VOIDED-AMT
           END-IF
           IF MORM-VOIDED-AMT > 100.00
               MOVE 100.00 TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-CLOSE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE FEE ON THE FEE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3300-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORT-FACE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORT-FACE-AMT ROUNDED = MORM-FEE-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-TYPE-CD = 'AGE'
                   MOVE 15.00 TO MORT-FACE-AMT
               WHEN MORM-TYPE-CD = 'JDX'
                   MOVE 5.00 TO MORT-FACE-AMT
               WHEN OTHER
                   COMPUTE MORT-FACE-AMT ROUNDED = MORM-FEE-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORT-FACE-AMT > 0 AND MORT-FACE-AMT < 1.00
               MOVE 1.00 TO MORT-FACE-AMT
           END-IF
           IF MORT-FACE-AMT > 250.00
               MOVE 250.00 TO MORT-FACE-AMT
           END-IF
           ADD MORT-FACE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-FEE.
           IF MORM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FEE-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORT-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-ESCHEAT.
           IF MORT-ESCHEAT-AMT < 10.00
               MOVE 10.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-ESCHEAT-AMT > 10000.00
               MOVE 10000.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE ESCHEAT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3330-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORM-PAID-YTD ROUNDED = MORM-ESCHEAT-AMT *
                       MORM-DISCOUNT / 200
               WHEN MORM-CYCLE-CD = 'DGU'
                   MOVE 12.00 TO MORM-PAID-YTD
               WHEN MORM-CYCLE-CD = 'GEJ'
                   MOVE 5.00 TO MORM-PAID-YTD
               WHEN OTHER
                   COMPUTE MORM-PAID-YTD ROUNDED = MORM-ESCHEAT-AMT *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORM-PAID-YTD > 0 AND MORM-PAID-YTD < 1.00
               MOVE 1.00 TO MORM-PAID-YTD
           END-IF
           IF MORM-PAID-YTD > 50.00
               MOVE 50.00 TO MORM-PAID-YTD
           END-IF
           ADD MORM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOIDED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-VOIDED.
           IF MORM-VOIDED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-VOIDED-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E168 REJECT CD REQUIRED
      * E993 FEE YTD NEGATIVE
      * E047 FEE YTD OVER 25000
      * E220 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN MORM-FEE-YTD < 0
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN MORM-FEE-YTD > 25000.00
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN MORM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-CHANNEL.
           EVALUATE MORT-CHANNEL-CD
               WHEN 'BRN'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3370-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-AMT < 0.00
               MOVE 0.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-AMT > 2500.00
               MOVE 2500.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-LAST-PAY.
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
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3400-ACCRUE-FACE.
           IF MORM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-YTD * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3410-FLAG-FACE.
           IF MORM-FACE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN OPEN DATE IS PAST
      * DX -> PT WHEN OPEN DATE IS PAST
      * PT -> RN WHEN OPEN DATE IS PAST
      * RN -> YD WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-EXPIRY.
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
      * DAILY INTEREST ON FACE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-FACE.
           IF MORM-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-AMT * MORM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOIDED AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-VOIDED-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-CHANNEL.
           EVALUATE MORT-CHANNEL-CD
               WHEN 'BRN'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO MORT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOIDED AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3490-LIMIT-VOIDED.
           IF MORM-VOIDED-AMT < 25.00
               MOVE 25.00 TO MORM-VOIDED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-VOIDED-AMT > 50000.00
               MOVE 50000.00 TO MORM-VOIDED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3500-LIMIT-ESCHEAT.
           IF MORT-ESCHEAT-AMT < 25.00
               MOVE 25.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-ESCHEAT-AMT > 500.00
               MOVE 500.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E995 REJECT CD REQUIRED
      * E343 OUTSTAND AMT NEGATIVE
      * E307 OUTSTAND AMT OVER 10000
      * E519 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN MORT-OUTSTAND-AMT < 0
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN MORT-OUTSTAND-AMT > 10000.00
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN MORM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-BAND-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-OUTSTAND-BAND-LOW(WS-TX)
                   MOVE WS-OUTSTAND-BAND-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MORM-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-ESCHEAT.
           IF MORM-ESCHEAT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E942 REF NO REQUIRED
      * E739 FEE AMT NEGATIVE
      * E207 FEE AMT OVER 250000
      * E889 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3560-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-REF-NO = SPACES
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN MORM-FEE-AMT < 0
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN MORM-FEE-AMT > 250000.00
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3570-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ES'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT FEE ON THE PAID YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3610-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORT-ESCHEAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = MORM-PAID-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 15.00 TO MORT-ESCHEAT-AMT
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 15.00 TO MORT-ESCHEAT-AMT
               WHEN OTHER
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = MORM-PAID-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORT-ESCHEAT-AMT > 0 AND MORT-ESCHEAT-AMT < 10.00
               MOVE 10.00 TO MORT-ESCHEAT-AMT
           END-IF
           IF MORT-ESCHEAT-AMT > 150.00
               MOVE 150.00 TO MORT-ESCHEAT-AMT
           END-IF
           ADD MORT-ESCHEAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-PAID.
           IF MORM-PAID-AMT < 50.00
               MOVE 50.00 TO MORM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-PAID-AMT > 5000.00
               MOVE 5000.00 TO MORM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-OPEN.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-LAST-PAY.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-PAID.
           IF MORM-PAID-YTD < 0.00
               MOVE 0.00 TO MORM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-PAID-YTD > 5000.00
               MOVE 5000.00 TO MORM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-SEQ.
           IF MORT-SEQ < 9999999
               ADD 1 TO MORT-SEQ
           END-IF
           IF MORT-SEQ NOT < 5
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-ITEM.
           IF MORM-ITEM-CNT < 9999999
               ADD 1 TO MORM-ITEM-CNT
           END-IF
           IF MORM-ITEM-CNT NOT < 3
               MOVE 'Y' TO MORM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOIDED AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-VOIDED-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN EFF DATE IS PAST
      * JDX -> LDX WHEN EFF DATE IS PAST
      * LDX -> RHT WHEN EFF DATE IS PAST
      * RHT -> AGE WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 10
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3750-FLAG-PAID.
           IF MORM-PAID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E623 MEMO REQUIRED
      * E530 ESCHEAT AMT NEGATIVE
      * E789 ESCHEAT AMT OVER 99999
      * E264 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-MEMO = SPACES
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT < 0
                   MOVE 'E530' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-AMT > 99999.00
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN MORM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E294 DESC REQUIRED
      * E634 FEE AMT NEGATIVE
      * E637 FEE AMT OVER 250000
      * E611 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN MORT-FEE-AMT < 0
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN MORT-FEE-AMT > 250000.00
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN MORM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E611' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3800-LIMIT-OUTSTAND.
           IF MORM-OUTSTAND-AMT < 100.00
               MOVE 100.00 TO MORM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-OUTSTAND-AMT > 500.00
               MOVE 500.00 TO MORM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ES'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3820-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ESCHEAT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3830-FLAG-ESCHEAT.
           IF MORM-ESCHEAT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-ESCHEAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-ITEM.
           IF MORM-ITEM-CNT < 9999999
               ADD 1 TO MORM-ITEM-CNT
           END-IF
           IF MORM-ITEM-CNT NOT < 5
               MOVE 'Y' TO MORM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MORT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN EFFECTIVE DATE IS PAST
      * GEJ -> JBE WHEN EFFECTIVE DATE IS PAST
      * JBE -> DGU WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3890-FLAG-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-OUTSTAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-PAID.
           IF MORM-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-PAID-AMT * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-VOIDED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOIDED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ESCHEAT AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-ESCHEAT-AMT ROUNDED =
                   MORM-VOIDED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-ESCHEAT-AMT
           END-IF
           ADD MORM-ESCHEAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-AMT - (WS-WORK-AMT * 2)
           COMPUTE MORM-ESCHEAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN LAST PAY DATE IS PAST
      * DX -> PT WHEN LAST PAY DATE IS PAST
      * PT -> RN WHEN LAST PAY DATE IS PAST
      * RN -> YD WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MORM-ESCHEAT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MORM-ESCHEAT-YTD - (WS-WORK-AMT * 12)
           COMPUTE MORM-VOIDED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-FEE.
           IF MORT-FEE-AMT < 25.00
               MOVE 25.00 TO MORT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FEE-AMT > 5000.00
               MOVE 5000.00 TO MORT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN LAST MAINT DATE IS PAST
      * L -> P WHEN LAST MAINT DATE IS PAST
      * P -> S WHEN LAST MAINT DATE IS PAST
      * S -> T WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'L' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'P' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-LAST-ACT.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E307 CONTACT REQUIRED
      * E299 OUTSTAND AMT NEGATIVE
      * E465 OUTSTAND AMT OVER 10000
      * E704 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN MORT-OUTSTAND-AMT < 0
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN MORT-OUTSTAND-AMT > 10000.00
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN MORT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4030-LIMIT-FACE.
           IF MORT-FACE-AMT < 25.00
               MOVE 25.00 TO MORT-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FACE-AMT > 10000.00
               MOVE 10000.00 TO MORT-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORT-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORT-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE ESCHEAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MORM-ESCHEAT-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-EXPIRY.
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
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE MORT-ESCHEAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-ITEM.
           IF MORM-ITEM-CNT < 9999999
               ADD 1 TO MORM-ITEM-CNT
           END-IF
           IF MORM-ITEM-CNT NOT < 10
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ES'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-CHANNEL.
           EVALUATE MORT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E282 DESC REQUIRED
      * E165 VOIDED AMT NEGATIVE
      * E334 VOIDED AMT OVER 50000
      * E102 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN MORM-VOIDED-AMT < 0
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN MORM-VOIDED-AMT > 50000.00
                   MOVE 'E334' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

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
                   MOVE MORISK-OUTSTAND-AMT
                     TO MORM-OUTSTAND-AMT
                   MOVE MORISK-PREFERRED-FLAG
                     TO MORM-PREFERRED-FLAG
                   MOVE MORISK-AUTO-RENEW-FLAG
                     TO MORM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-MORISK' TO ABND-PARAGRAPH
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
                   MOVE MODTE-FEE-YTD
                     TO MORM-FEE-YTD
                   MOVE MODTE-FEE-AMT
                     TO MORM-FEE-AMT
                   MOVE MODTE-ESCHEAT-AMT
                     TO MORM-ESCHEAT-AMT
                   MOVE MODTE-AUTO-RENEW-FLAG
                     TO MORM-AUTO-RENEW-FLAG
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
           DISPLAY 'MORPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MORPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
