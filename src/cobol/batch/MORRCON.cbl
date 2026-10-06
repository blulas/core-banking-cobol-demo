      *================================================================*
      * PROGRAM  : MORRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MONEY ORDERS - RECONCILIATION. READS THE MOR        *
      *            TRANSACTIONS, UPDATES THE MOR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. MONEY ORDER AND OFFICIAL CHECK  *
      *            ISSUANCE, PAID ITEMS AND ESCHEAT                    *
      * INPUT    : MORMST (KSDS), MORTRN, BANKCTL                      *
      * OUTPUT   : MOROUT, MORRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MOFEE, MORISK           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-10 LMB  ORIGINAL                                       *
      * 1999-08-24 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-10-24 JLM  NEW FEE REPORT COLUMNS CHG2314                 *
      * 2004-05-27 CRP  OUTSTAND RERUN PROTECTION INC1307              *
      * 2006-01-07 JLM  VOIDED CUTOFF MOVED PRB7745                    *
      * 2006-10-14 BHS  OUTSTAND TABLE EXPANDED CHG2515                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORRCON.
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
           SELECT REPORT-FILE ASSIGN TO "MORRCRPT"
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
                                           VALUE 19.000000.
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
      * WS-VOIDED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOIDED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000611000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000306500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000590500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000590000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000124500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016300000000435000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000005000'.
       01  WS-VOIDED-GRID-TBL REDEFINES WS-VOIDED-GRID-VALUES.
           05  WS-VOIDED-GRID-ROW OCCURS 7 TIMES.
               10  WS-VOIDED-GRID-LOW      PIC 9(09)V99.
               10  WS-VOIDED-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOIDED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOIDED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000179000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000186500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000419500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000794500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000611000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005450000000330500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000777000'.
       01  WS-VOIDED-TIER-TBL REDEFINES WS-VOIDED-TIER-VALUES.
           05  WS-VOIDED-TIER-ROW OCCURS 8 TIMES.
               10  WS-VOIDED-TIER-LOW      PIC 9(09)V99.
               10  WS-VOIDED-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ESCHEAT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ESCHEAT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000750000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000562500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000858000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000259500'.
       01  WS-ESCHEAT-TIER-TBL REDEFINES WS-ESCHEAT-TIER-VALUES.
           05  WS-ESCHEAT-TIER-ROW OCCURS 4 TIMES.
               10  WS-ESCHEAT-TIER-LOW     PIC 9(09)V99.
               10  WS-ESCHEAT-TIER-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MORRCON  MONEY ORDERS'.
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
       COPY MOFEEP.
       COPY MORISKP.

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
               PERFORM 3000-ACCRUE-PAID
               PERFORM 3010-SELECT-PENALTY-PCT
               PERFORM 3020-SELECT-PENALTY-PCT
               PERFORM 3030-SET-TYPE
               PERFORM 3040-LIMIT-ESCHEAT
               PERFORM 3050-EDIT-CONTACT
               PERFORM 3060-SELECT-PENALTY-PCT
               PERFORM 3070-CHECK-NOTICE
               PERFORM 3080-AGE-LAST-MAINT
               PERFORM 3090-AGE-NOTICE
               PERFORM 3100-ASSESS-OVERRIDE
               PERFORM 3110-CHECK-EFFECTIVE
               PERFORM 3120-AGE-LAST-PAY
               PERFORM 3130-LIMIT-FACE
               PERFORM 3140-AGE-OPEN
               PERFORM 3150-LIMIT-ESCHEAT
               PERFORM 3160-AGE-NOTICE
               PERFORM 3170-COUNT-RETRY
               PERFORM 3180-LIMIT-PAID
               PERFORM 3190-AGE-LAST-ACT
               PERFORM 3200-ACCUM-CHANNEL
               PERFORM 3210-ACCRUE-ESCHEAT
               PERFORM 3220-AGE-EXPIRY
               PERFORM 3230-ASSESS-EXEMPT
               PERFORM 3240-LIMIT-FACE
               PERFORM 3250-ACCRUE-OUTSTAND
               PERFORM 3260-FLAG-OUTSTAND
               PERFORM 3270-EDIT-REF-NO
               PERFORM 3280-FLAG-OUTSTAND
               PERFORM 3290-ACCUM-REASON
               PERFORM 3300-AGE-EFFECTIVE
               PERFORM 3310-CHECK-NOTICE
               PERFORM 3320-ASSESS-REVIEW
               PERFORM 3330-FLAG-VOIDED
               PERFORM 3340-FLAG-OUTSTAND
               PERFORM 3350-SET-ITEM-TYPE
               PERFORM 3360-SPLIT-PAID
               PERFORM 3370-PRORATE-ESCHEAT
               PERFORM 3380-AGE-LAST-MAINT
               PERFORM 3390-LIMIT-FACE
               PERFORM 3400-AGE-POST
               PERFORM 3410-LIMIT-FACE
               PERFORM 3420-COUNT-NOTICE
               PERFORM 3430-LIMIT-OUTSTAND
               PERFORM 3440-DESCRIBE-ITEM-TYPE
               PERFORM 3450-ACCRUE-FEE-PCT
               PERFORM 3460-AGE-LAST-STMT
               PERFORM 3470-FLAG-PAID
               PERFORM 3480-ACCUM-CHANNEL
               PERFORM 3490-SPLIT-VOIDED
               PERFORM 3500-CHECK-CLOSE
               PERFORM 3510-ACCRUE-FEE-PCT
               PERFORM 3520-PRORATE-FACE
               PERFORM 3530-DESCRIBE-CYCLE
               PERFORM 3540-FLAG-FEE
               PERFORM 3550-LIMIT-ESCHEAT
               PERFORM 3560-PRORATE-PAID
               PERFORM 3570-ACCRUE-OUTSTAND
               PERFORM 3580-SELECT-FEE-PCT
               PERFORM 3590-EDIT-DESC
               PERFORM 3600-SELECT-PENALTY-PCT
               PERFORM 3610-PRORATE-VOIDED
               PERFORM 3620-SPLIT-FEE
               PERFORM 3630-SET-REASON
               PERFORM 3640-PRORATE-VOIDED
               PERFORM 3650-SET-ITEM-TYPE
               PERFORM 3660-ASSESS-AUTO-RENEW
               PERFORM 3670-DESCRIBE-TRAN
               PERFORM 3680-EDIT-REJECT
               PERFORM 3690-ACCUM-TYPE
               PERFORM 3700-ACCRUE-FACE
               PERFORM 3710-ACCUM-CHANNEL
               PERFORM 3720-SELECT-FEE-PCT
               PERFORM 3730-DESCRIBE-TYPE
               PERFORM 3740-AGE-NOTICE
               PERFORM 3750-DESCRIBE-REASON
               PERFORM 3760-CHECK-EFF
               PERFORM 3770-SET-CHANNEL
               PERFORM 3780-CHECK-LAST-MAINT
               PERFORM 3790-FLAG-VOIDED
               PERFORM 3800-AGE-NOTICE
               PERFORM 3810-FLAG-PAID
               PERFORM 3820-SPLIT-VOIDED
               PERFORM 3830-AGE-EFF
               PERFORM 3840-PRORATE-PAID
               PERFORM 3850-PRORATE-FEE
               PERFORM 3860-SELECT-PENALTY-PCT
               PERFORM 3870-ACCUM-CHANNEL
               PERFORM 3880-SPLIT-ESCHEAT
               PERFORM 3890-ASSESS-REVIEW
               PERFORM 3900-SET-CHANNEL
               PERFORM 3910-CHECK-EXPIRY
               PERFORM 3920-AGE-EFFECTIVE
               PERFORM 3930-ACCRUE-PENALTY-PCT
               PERFORM 3940-AGE-OPEN
               PERFORM 3950-ACCUM-REASON
               PERFORM 3960-SPLIT-FEE
               PERFORM 3970-FLAG-OUTSTAND
               PERFORM 3980-AGE-POST
               PERFORM 3990-SPLIT-FEE
               PERFORM 4000-DESCRIBE-CYCLE
               PERFORM 4010-COUNT-NOTICE
               PERFORM 4020-ACCUM-TRAN
               PERFORM 4030-PRORATE-FEE
               PERFORM 4040-LIMIT-ITEM
               PERFORM 4050-LIMIT-FEE
               PERFORM 4060-PRORATE-FACE
               PERFORM 4070-EDIT-CUST-ID
               PERFORM 4080-PRORATE-FEE
               PERFORM 4090-ACCUM-CYCLE
               PERFORM 4100-ACCUM-ITEM-TYPE
               PERFORM 4110-LIMIT-FEE
               PERFORM 4120-FLAG-ESCHEAT
               PERFORM 4130-ACCUM-TRAN
               PERFORM 4140-SET-TYPE
               PERFORM 4150-PRORATE-VOIDED
               PERFORM 4160-SELECT-DISCOUNT
               PERFORM 4170-ASSESS-STOP
               PERFORM 4180-AGE-LAST-STMT
               PERFORM 4190-PRORATE-ESCHEAT
               PERFORM 4200-AGE-LAST-MAINT
               PERFORM 4210-AGE-EFF
               PERFORM 4220-LIMIT-FACE
               PERFORM 4230-SELECT-DISCOUNT
               PERFORM 4240-ACCUM-CHANNEL
               PERFORM 4250-SELECT-FEE-PCT
               PERFORM 4260-CHECK-EFF
               PERFORM 4270-SELECT-FEE-PCT
               PERFORM 4280-ACCUM-TYPE
               PERFORM 4290-CHECK-EXPIRY
               PERFORM 4300-EDIT-REF-NO
               PERFORM 4310-DESCRIBE-PAYEE-TYPE
               PERFORM 4320-LIMIT-OUTSTAND
               PERFORM 4330-FLAG-FACE
               PERFORM 4340-ACCUM-TYPE
               PERFORM 4350-SET-TRAN
               PERFORM 4360-ACCUM-PAYEE-TYPE
               PERFORM 4370-ASSESS-REVERSAL
               PERFORM 4380-PRORATE-FACE
               PERFORM 4390-AGE-LAST-MAINT
               PERFORM 4400-SET-CYCLE
               PERFORM 4410-ACCRUE-FACE
               PERFORM 4420-ACCRUE-OUTSTAND
               PERFORM 4430-PRORATE-ITEM
               PERFORM 4440-DESCRIBE-TYPE
               PERFORM 4450-SPLIT-ESCHEAT
               PERFORM 4460-DESCRIBE-CHANNEL
               PERFORM 4470-AGE-EFF
               PERFORM 4480-CHECK-CLOSE
               PERFORM 4490-SET-REASON
               PERFORM 4500-SELECT-FEE-PCT
               PERFORM 4510-COUNT-SEQ
               PERFORM 4520-SELECT-PENALTY-PCT
               PERFORM 4530-AGE-NOTICE
               PERFORM 4540-DESCRIBE-REASON
               PERFORM 4550-CHECK-POST
               PERFORM 4560-COUNT-RETRY
               PERFORM 4570-ACCUM-PAYEE-TYPE
               PERFORM 4580-ACCRUE-FEE
               PERFORM 4590-ASSESS-EXEMPT
               PERFORM 4600-ACCUM-ITEM-TYPE
               PERFORM 4610-FLAG-ESCHEAT
               PERFORM 4620-DESCRIBE-REASON
               PERFORM 4630-AGE-NOTICE
               PERFORM 4640-ACCUM-TYPE
               PERFORM 4650-CHECK-LAST-PAY
               PERFORM 4660-ACCRUE-FEE
               PERFORM 4670-COUNT-RETRY
               PERFORM 4680-SET-TYPE
               PERFORM 4690-SPLIT-FACE
               PERFORM 4700-ACCUM-CHANNEL
               PERFORM 4710-SELECT-PENALTY-PCT
               PERFORM 4720-LIMIT-OUTSTAND
               PERFORM 4730-DESCRIBE-CHANNEL
               PERFORM 4740-DESCRIBE-CYCLE
               PERFORM 4750-ACCRUE-ESCHEAT
               PERFORM 4760-DESCRIBE-ITEM-TYPE
               PERFORM 7000-CALL-MOFEE
               PERFORM 7010-CALL-MORISK
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
      * DAILY INTEREST ON PAID YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-PAID.
           IF MORM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-PAID-YTD * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-GRID-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORT-FEE-AMT NOT < WS-VOIDED-GRID-LOW(WS-TX)
                   MOVE WS-VOIDED-GRID-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MORM-FEE-AMT NOT < WS-ESCHEAT-TIER-LOW(WS-TX)
                   MOVE WS-ESCHEAT-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN NOTICE DATE IS PAST
      * JDX -> LDX WHEN NOTICE DATE IS PAST
      * LDX -> RHT WHEN NOTICE DATE IS PAST
      * RHT -> AGE WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ESCHEAT YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3040-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-YTD < 0.00
               MOVE 0.00 TO MORM-ESCHEAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-YTD > 1000.00
               MOVE 1000.00 TO MORM-ESCHEAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E094 CONTACT REQUIRED
      * E458 OUTSTAND YTD NEGATIVE
      * E295 OUTSTAND YTD OVER 250000
      * E911 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-YTD < 0
                   MOVE 'E458' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-YTD > 250000.00
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN MORM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED GRID TIER FOR THE ESCHEAT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-GRID-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-ESCHEAT-YTD NOT < WS-VOIDED-GRID-LOW(WS-TX)
                   MOVE WS-VOIDED-GRID-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-NOTICE.
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
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-LAST-MAINT.
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
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-NOTICE.
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
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE ESCHEAT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3100-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MORT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-STOP-FLAG = 'Y'
                   COMPUTE MORT-FEE-AMT ROUNDED = MORT-ESCHEAT-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 15.00 TO MORT-FEE-AMT
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 20.00 TO MORT-FEE-AMT
               WHEN OTHER
                   COMPUTE MORT-FEE-AMT ROUNDED = MORT-ESCHEAT-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORT-FEE-AMT > 0 AND MORT-FEE-AMT < 1.00
               MOVE 1.00 TO MORT-FEE-AMT
           END-IF
           IF MORT-FEE-AMT > 150.00
               MOVE 150.00 TO MORT-FEE-AMT
           END-IF
           ADD MORT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-LAST-PAY.
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
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-FACE.
           IF MORT-FACE-AMT < 10.00
               MOVE 10.00 TO MORT-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FACE-AMT > 10000.00
               MOVE 10000.00 TO MORT-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-OPEN.
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
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3150-LIMIT-ESCHEAT.
           IF MORT-ESCHEAT-AMT < 100.00
               MOVE 100.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-ESCHEAT-AMT > 500.00
               MOVE 500.00 TO MORT-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-NOTICE.
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
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-ESCHEAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 5
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-PAID.
           IF MORM-PAID-YTD < 100.00
               MOVE 100.00 TO MORM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-PAID-YTD > 1000.00
               MOVE 1000.00 TO MORM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-LAST-ACT.
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
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-CHANNEL.
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
      * DAILY INTEREST ON ESCHEAT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-EXPIRY.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE FACE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-FACE-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-TYPE-CD = 'AGE'
                   MOVE 20.00 TO MORM-FEE-AMT
               WHEN MORM-TYPE-CD = 'JDX'
                   MOVE 7.00 TO MORM-FEE-AMT
               WHEN OTHER
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-FACE-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-FEE-AMT > 0 AND MORM-FEE-AMT < 2.00
               MOVE 2.00 TO MORM-FEE-AMT
           END-IF
           IF MORM-FEE-AMT > 50.00
               MOVE 50.00 TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3240-LIMIT-FACE.
           IF MORM-FACE-AMT < 50.00
               MOVE 50.00 TO MORM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FACE-AMT > 10000.00
               MOVE 10000.00 TO MORM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * MORM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3260-FLAG-OUTSTAND.
           IF MORM-OUTSTAND-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E680 REF NO REQUIRED
      * E330 OUTSTAND AMT NEGATIVE
      * E911 OUTSTAND AMT OVER 5000
      * E306 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-REF-NO = SPACES
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-AMT < 0
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-AMT > 5000.00
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3280-FLAG-OUTSTAND.
           IF MORM-OUTSTAND-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-REASON-CD
                   ADD MORM-ESCHEAT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-EFFECTIVE.
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
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOIDED FEE ON THE FACE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3320-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MORM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MORM-VOIDED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   COMPUTE MORM-VOIDED-AMT ROUNDED = MORM-FACE-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 12.00 TO MORM-VOIDED-AMT
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 7.00 TO MORM-VOIDED-AMT
               WHEN OTHER
                   COMPUTE MORM-VOIDED-AMT ROUNDED = MORM-FACE-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-VOIDED-AMT > 0 AND MORM-VOIDED-AMT < 2.00
               MOVE 2.00 TO MORM-VOIDED-AMT
           END-IF
           IF MORM-VOIDED-AMT > 25.00
               MOVE 25.00 TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * VOIDED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3330-FLAG-VOIDED.
           IF MORM-VOIDED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-VOIDED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3340-FLAG-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-OUTSTAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN CLOSE DATE IS PAST
      * H -> S WHEN CLOSE DATE IS PAST
      * S -> T WHEN CLOSE DATE IS PAST
      * T -> B WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = MORM-PAID-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MORM-PAID-YTD - (WS-WORK-AMT * 4)
           COMPUTE MORM-FACE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ESCHEAT AMT.
      *----------------------------------------------------------------*
       3370-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-ESCHEAT-AMT ROUNDED =
                   MORT-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORT-ESCHEAT-AMT
           END-IF
           ADD MORT-ESCHEAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-LAST-MAINT.
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
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3390-LIMIT-FACE.
           IF MORT-FACE-AMT < 50.00
               MOVE 50.00 TO MORT-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FACE-AMT > 50000.00
               MOVE 50000.00 TO MORT-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-POST.
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
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-FACE.
           IF MORT-FACE-AMT < 50.00
               MOVE 50.00 TO MORT-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FACE-AMT > 5000.00
               MOVE 5000.00 TO MORT-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-OUTSTAND.
           IF MORM-OUTSTAND-AMT < 0.00
               MOVE 0.00 TO MORM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-OUTSTAND-AMT > 5000.00
               MOVE 5000.00 TO MORM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-LAST-STMT.
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
      * PAID AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3470-FLAG-PAID.
           IF MORM-PAID-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-CHANNEL.
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
      * SPLIT VOIDED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-CLOSE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-OUTSTAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3520-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-FACE-AMT ROUNDED =
                   MORM-FACE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-FACE-AMT
           END-IF
           ADD MORM-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-FEE.
           IF MORM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3550-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-YTD < 0.00
               MOVE 0.00 TO MORM-ESCHEAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-YTD > 2500.00
               MOVE 2500.00 TO MORM-ESCHEAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3560-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-PAID-YTD ROUNDED =
                   MORM-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MORM-PAID-YTD
           END-IF
           ADD MORM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-OUTSTAND.
           IF MORT-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-OUTSTAND-AMT * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED GRID TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-GRID-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-OUTSTAND-AMT NOT < WS-VOIDED-GRID-LOW(WS-TX)
                   MOVE WS-VOIDED-GRID-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E816 DESC REQUIRED
      * E921 FACE AMT NEGATIVE
      * E942 FACE AMT OVER 1000000
      * E742 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT < 0
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT > 1000000.00
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-GRID-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-FEE-AMT NOT < WS-VOIDED-GRID-LOW(WS-TX)
                   MOVE WS-VOIDED-GRID-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOIDED AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-VOIDED-AMT ROUNDED =
                   MORM-ESCHEAT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE MORM-VOIDED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN LAST PAY DATE IS PAST
      * L -> P WHEN LAST PAY DATE IS PAST
      * P -> S WHEN LAST PAY DATE IS PAST
      * S -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOIDED YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-VOIDED-YTD ROUNDED =
                   MORM-ESCHEAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-VOIDED-YTD
           END-IF
           ADD MORM-VOIDED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN EXPIRY DATE IS PAST
      * H -> S WHEN EXPIRY DATE IS PAST
      * S -> T WHEN EXPIRY DATE IS PAST
      * T -> B WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT YTD FEE ON THE FACE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3660-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MORM-ESCHEAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   COMPUTE MORM-ESCHEAT-YTD ROUNDED = MORM-FACE-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 5.00 TO MORM-ESCHEAT-YTD
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 5.00 TO MORM-ESCHEAT-YTD
               WHEN OTHER
                   COMPUTE MORM-ESCHEAT-YTD ROUNDED = MORM-FACE-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-ESCHEAT-YTD > 0 AND MORM-ESCHEAT-YTD < 1.00
               MOVE 1.00 TO MORM-ESCHEAT-YTD
           END-IF
           IF MORM-ESCHEAT-YTD > 35.00
               MOVE 35.00 TO MORM-ESCHEAT-YTD
           END-IF
           ADD MORM-ESCHEAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ES'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E095 REJECT CD REQUIRED
      * E362 FACE AMT NEGATIVE
      * E291 FACE AMT OVER 10000
      * E181 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT < 0
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT > 10000.00
                   MOVE 'E291' TO WS-REJECT-CODE
               WHEN MORM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-FACE.
           IF MORM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-YTD * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
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
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JDX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LDX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RHT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-NOTICE.
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
               WHEN WS-DAYS NOT > 60
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-EFF.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN LAST ACT DATE IS PAST
      * DX -> PT WHEN LAST ACT DATE IS PAST
      * PT -> RN WHEN LAST ACT DATE IS PAST
      * RN -> YD WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOIDED YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3790-FLAG-VOIDED.
           IF MORM-VOIDED-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-VOIDED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-NOTICE.
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
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3810-FLAG-PAID.
           IF MORM-PAID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOIDED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-YTD - (WS-WORK-AMT * 4)
           COMPUTE MORM-FACE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3840-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-PAID-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-PAID-AMT
           END-IF
           ADD MORM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VOIDED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3850-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-FEE-YTD ROUNDED =
                   MORM-VOIDED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-FEE-YTD
           END-IF
           ADD MORM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MORM-FEE-YTD NOT < WS-ESCHEAT-TIER-LOW(WS-TX)
                   MOVE WS-ESCHEAT-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE MORM-ESCHEAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE PAID YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3890-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MORM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MORM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-PAID-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 15.00 TO MORM-FEE-AMT
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 15.00 TO MORM-FEE-AMT
               WHEN OTHER
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-PAID-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-FEE-AMT > 0 AND MORM-FEE-AMT < 1.00
               MOVE 1.00 TO MORM-FEE-AMT
           END-IF
           IF MORM-FEE-AMT > 100.00
               MOVE 100.00 TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN POST DATE IS PAST
      * DX -> PT WHEN POST DATE IS PAST
      * PT -> RN WHEN POST DATE IS PAST
      * RN -> YD WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-EXPIRY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-EFFECTIVE.
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
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3940-AGE-OPEN.
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
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-REASON-CD
                   ADD MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MORT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3970-FLAG-OUTSTAND.
           IF MORT-OUTSTAND-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORT-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-POST.
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
               WHEN WS-DAYS NOT > 30
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MORM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-TRAN-CD
                   ADD MORM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4030-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-FEE-AMT ROUNDED =
                   MORM-FACE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORT-FEE-AMT
           END-IF
           ADD MORT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4040-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4050-LIMIT-FEE.
           IF MORM-FEE-AMT < 0.00
               MOVE 0.00 TO MORM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FEE-AMT > 500.00
               MOVE 500.00 TO MORM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOIDED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-FACE-AMT ROUNDED =
                   MORM-VOIDED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORT-FACE-AMT
           END-IF
           ADD MORT-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E790 CUST ID REQUIRED
      * E999 ESCHEAT YTD NEGATIVE
      * E506 ESCHEAT YTD OVER 1000000
      * E962 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E790' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-YTD < 0
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN MORM-ESCHEAT-YTD > 1000000.00
                   MOVE 'E506' TO WS-REJECT-CODE
               WHEN MORM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4080-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-FEE-YTD ROUNDED =
                   MORM-OUTSTAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-FEE-YTD
           END-IF
           ADD MORM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-CYCLE-CD
                   ADD MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-ITEM-TYPE.
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
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-FEE.
           IF MORM-FEE-YTD < 0.00
               MOVE 0.00 TO MORM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FEE-YTD > 10000.00
               MOVE 10000.00 TO MORM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4120-FLAG-ESCHEAT.
           IF MORM-ESCHEAT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN EFF DATE IS PAST
      * JDX -> LDX WHEN EFF DATE IS PAST
      * LDX -> RHT WHEN EFF DATE IS PAST
      * RHT -> AGE WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-TYPE.
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
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOIDED AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-VOIDED-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-ESCHEAT-TIER-LOW(WS-TX)
                   MOVE WS-ESCHEAT-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE FACE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4170-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORT-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORM-FACE-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO MORT-OUTSTAND-AMT
               WHEN MORT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO MORT-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORM-FACE-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORT-OUTSTAND-AMT > 0 AND MORT-OUTSTAND-AMT < 10.00
               MOVE 10.00 TO MORT-OUTSTAND-AMT
           END-IF
           IF MORT-OUTSTAND-AMT > 250.00
               MOVE 250.00 TO MORT-OUTSTAND-AMT
           END-IF
           ADD MORT-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-LAST-STMT.
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
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-VOIDED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ESCHEAT AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-ESCHEAT-AMT ROUNDED =
                   MORM-ESCHEAT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-ESCHEAT-AMT
           END-IF
           ADD MORM-ESCHEAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-LAST-MAINT.
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
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4210-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FACE YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4220-LIMIT-FACE.
           IF MORM-FACE-YTD < 10.00
               MOVE 10.00 TO MORM-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FACE-YTD > 2500.00
               MOVE 2500.00 TO MORM-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT TIER TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MORM-OUTSTAND-AMT NOT < WS-ESCHEAT-TIER-LOW(WS-TX)
                   MOVE WS-ESCHEAT-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-CHANNEL.
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
      * SELECT THE ESCHEAT TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-TIER-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-ESCHEAT-TIER-LOW(WS-TX)
                   MOVE WS-ESCHEAT-TIER-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-GRID-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-FEE-YTD NOT < WS-VOIDED-GRID-LOW(WS-TX)
                   MOVE WS-VOIDED-GRID-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORM-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-EXPIRY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E592 REF NO REQUIRED
      * E762 FEE AMT NEGATIVE
      * E272 FEE AMT OVER 99999
      * E945 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-REF-NO = SPACES
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN MORM-FEE-AMT < 0
                   MOVE 'E762' TO WS-REJECT-CODE
               WHEN MORM-FEE-AMT > 99999.00
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN MORM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4310-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4320-LIMIT-OUTSTAND.
           IF MORM-OUTSTAND-YTD < 10.00
               MOVE 10.00 TO MORM-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-OUTSTAND-YTD > 5000.00
               MOVE 5000.00 TO MORM-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4330-FLAG-FACE.
           IF MORT-FACE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORT-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN EXPIRY DATE IS PAST
      * PD -> VD WHEN EXPIRY DATE IS PAST
      * VD -> SP WHEN EXPIRY DATE IS PAST
      * SP -> ES WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4350-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-PAYEE-TYPE-CD
                   ADD MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT FEE ON THE ESCHEAT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4370-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MORT-ESCHEAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = MORM-ESCHEAT-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-PAYEE-TYPE-CD = 'CJ'
                   MOVE 7.00 TO MORT-ESCHEAT-AMT
               WHEN MORM-PAYEE-TYPE-CD = 'FY'
                   MOVE 5.00 TO MORT-ESCHEAT-AMT
               WHEN OTHER
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = MORM-ESCHEAT-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORT-ESCHEAT-AMT > 0 AND MORT-ESCHEAT-AMT < 2.00
               MOVE 2.00 TO MORT-ESCHEAT-AMT
           END-IF
           IF MORT-ESCHEAT-AMT > 35.00
               MOVE 35.00 TO MORT-ESCHEAT-AMT
           END-IF
           ADD MORT-ESCHEAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4380-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4390-AGE-LAST-MAINT.
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
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN EFF DATE IS PAST
      * GEJ -> JBE WHEN EFF DATE IS PAST
      * JBE -> DGU WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4410-ACCRUE-FACE.
           IF MORM-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-AMT * MORM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-OUTSTAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4420-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-AMT * MORM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-OUTSTAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4430-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4440-DESCRIBE-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JDX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LDX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RHT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MORT-ESCHEAT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORT-ESCHEAT-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORT-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4460-DESCRIBE-CHANNEL.
           EVALUATE MORT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4470-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-CLOSE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MORT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN LAST PAY DATE IS PAST
      * L -> P WHEN LAST PAY DATE IS PAST
      * P -> S WHEN LAST PAY DATE IS PAST
      * S -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4490-SET-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4500-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-VOIDED-TIER-LOW(WS-TX)
                   MOVE WS-VOIDED-TIER-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4510-COUNT-SEQ.
           IF MORT-SEQ < 9999999
               ADD 1 TO MORT-SEQ
           END-IF
           IF MORT-SEQ NOT < 10
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT TIER TIER FOR THE ESCHEAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4520-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MORT-ESCHEAT-AMT NOT < WS-ESCHEAT-TIER-LOW(WS-TX)
                   MOVE WS-ESCHEAT-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4530-AGE-NOTICE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4540-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4550-CHECK-POST.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4560-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 12
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4570-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-PAYEE-TYPE-CD
                   ADD MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4580-ACCRUE-FEE.
           IF MORM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FEE-YTD * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4590-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORT-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORT-FEE-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORM-TYPE-CD = 'AGE'
                   MOVE 5.00 TO MORT-OUTSTAND-AMT
               WHEN MORM-TYPE-CD = 'JDX'
                   MOVE 3.00 TO MORT-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORT-FEE-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORT-OUTSTAND-AMT > 0 AND MORT-OUTSTAND-AMT < 1.00
               MOVE 1.00 TO MORT-OUTSTAND-AMT
           END-IF
           IF MORT-OUTSTAND-AMT > 25.00
               MOVE 25.00 TO MORT-OUTSTAND-AMT
           END-IF
           ADD MORT-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE VOIDED AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4600-ACCUM-ITEM-TYPE.
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
      * ESCHEAT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4610-FLAG-ESCHEAT.
           IF MORM-ESCHEAT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4620-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4630-AGE-NOTICE.
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
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4640-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORM-ESCHEAT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4650-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-PAY-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4660-ACCRUE-FEE.
           IF MORT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-FEE-AMT * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORT-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4670-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 3
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN NOTICE DATE IS PAST
      * JDX -> LDX WHEN NOTICE DATE IS PAST
      * LDX -> RHT WHEN NOTICE DATE IS PAST
      * RHT -> AGE WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4680-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4690-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE MORT-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4700-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED TIER TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4710-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MORM-PAID-AMT NOT < WS-VOIDED-TIER-LOW(WS-TX)
                   MOVE WS-VOIDED-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4720-LIMIT-OUTSTAND.
           IF MORM-OUTSTAND-AMT < 0.00
               MOVE 0.00 TO MORM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-OUTSTAND-AMT > 500.00
               MOVE 500.00 TO MORM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4730-DESCRIBE-CHANNEL.
           EVALUATE MORT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4740-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4750-ACCRUE-ESCHEAT.
           IF MORM-ESCHEAT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-ESCHEAT-YTD * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORT-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4760-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
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
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-MOFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE MORISK-OUTSTAND-YTD
                     TO MORM-OUTSTAND-YTD
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
           DISPLAY 'MORRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MORRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
