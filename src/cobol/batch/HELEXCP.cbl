      *================================================================*
      * PROGRAM  : HELEXCP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : HOME EQUITY LINES - EXCEPTION PROCESSING. READS THE *
      *            HEL TRANSACTIONS, UPDATES THE HEL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. HOME EQUITY LINE DRAWS,*
      *            BILLING, RATE CHANGE AND FREEZE                     *
      * INPUT    : HELMST (KSDS), HELTRN, BANKCTL                      *
      * OUTPUT   : HELOUT, HELEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, HEWAIV, HELIM, HERISK   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-05 BHS  ORIGINAL                                       *
      * 2000-07-26 LMB  LIMIT ROUNDING - HALF UP REQ3882               *
      * 2003-06-12 CRP  APPRAISE ROUNDING - HALF UP INC2069            *
      * 2006-01-04 CRP  LIMIT TABLE EXPANDED PRB5349                   *
      * 2006-07-22 JLM  APPRAISE FEE SCHEDULE UPDATE INC8463           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELEXCP.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "HELMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS HELM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "HELTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "HELOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "HELEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY HELMST.
       FD  TRAN-FILE.
       COPY HELTRN.
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
      * WS-BALANCE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BALANCE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000498000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000322500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000197000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000848000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000438500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000834500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000807500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009050000000536000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009300000000472500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009350000000338000'.
       01  WS-BALANCE-BAND-TBL REDEFINES WS-BALANCE-BAND-VALUES.
           05  WS-BALANCE-BAND-ROW OCCURS 10 TIMES.
               10  WS-BALANCE-BAND-LOW     PIC 9(09)V99.
               10  WS-BALANCE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LIMIT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LIMIT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000339500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000553500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000761000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000841000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015150000000463000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015200000000782000'.
       01  WS-LIMIT-GRID-TBL REDEFINES WS-LIMIT-GRID-VALUES.
           05  WS-LIMIT-GRID-ROW OCCURS 6 TIMES.
               10  WS-LIMIT-GRID-LOW       PIC 9(09)V99.
               10  WS-LIMIT-GRID-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  HELEXCP  HOME EQUITY LINES'.
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
       COPY HEWAIVP.
       COPY HELIMP.
       COPY HERISKP.

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
               MOVE 'HELMST' TO ABND-FILE-NAME
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
           MOVE HELT-KEY TO HELM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-EDIT-MEMO
               PERFORM 3010-DESCRIBE-CHANNEL
               PERFORM 3020-SPLIT-DRAW
               PERFORM 3030-PRORATE-MIN-PAY
               PERFORM 3040-CHECK-EXPIRY
               PERFORM 3050-SELECT-PENALTY-PCT
               PERFORM 3060-ASSESS-OVERRIDE
               PERFORM 3070-LIMIT-APPRAISE
               PERFORM 3080-EDIT-ADDR
               PERFORM 3090-SELECT-MARGIN
               PERFORM 3100-PRORATE-APPRAISE
               PERFORM 3110-LIMIT-INTEREST
               PERFORM 3120-SELECT-MARGIN
               PERFORM 3130-PRORATE-DRAW
               PERFORM 3140-EDIT-ADDR
               PERFORM 3150-LIMIT-LIMIT
               PERFORM 3160-ACCUM-REGION
               PERFORM 3170-COUNT-SEQ
               PERFORM 3180-EDIT-OPERATOR
               PERFORM 3190-ASSESS-SUSPEND
               PERFORM 3200-PRORATE-LIMIT
               PERFORM 3210-SPLIT-LIMIT
               PERFORM 3220-EDIT-NAME
               PERFORM 3230-CHECK-LAST-PAY
               PERFORM 3240-AGE-EFF
               PERFORM 3250-LIMIT-DRAW
               PERFORM 3260-LIMIT-ANNUAL-FEE
               PERFORM 3270-ACCUM-TRAN
               PERFORM 3280-SPLIT-APPRAISE
               PERFORM 3290-SET-TRAN
               PERFORM 3300-COUNT-RETRY
               PERFORM 3310-COUNT-PAYMENT
               PERFORM 3320-ASSESS-AUTO-RENEW
               PERFORM 3330-PRORATE-INTEREST
               PERFORM 3340-ASSESS-HOLD
               PERFORM 3350-CHECK-EFF
               PERFORM 3360-COUNT-PAYMENT
               PERFORM 3370-PRORATE-ANNUAL-FEE
               PERFORM 3380-FLAG-LIMIT
               PERFORM 3390-FLAG-MIN-PAY
               PERFORM 3400-PRORATE-LIMIT
               PERFORM 3410-DESCRIBE-TYPE
               PERFORM 3420-FLAG-INTEREST
               PERFORM 3430-COUNT-EXCEPT
               PERFORM 3440-ACCRUE-ANNUAL-FEE
               PERFORM 3450-SELECT-MARGIN
               PERFORM 3460-SELECT-MARGIN
               PERFORM 3470-SPLIT-DRAW
               PERFORM 3480-ACCRUE-LIMIT
               PERFORM 3490-AGE-EXPIRY
               PERFORM 3500-ACCRUE-ANNUAL-FEE
               PERFORM 3510-FLAG-INTEREST
               PERFORM 3520-FLAG-ITEM
               PERFORM 3530-ASSESS-SUSPEND
               PERFORM 3540-EDIT-REF-NO
               PERFORM 3550-SPLIT-ANNUAL-FEE
               PERFORM 3560-COUNT-RETRY
               PERFORM 3570-CHECK-NEXT-REVIEW
               PERFORM 3580-ACCUM-DRAW-PERIOD
               PERFORM 3590-AGE-NEXT-REVIEW
               PERFORM 3600-LIMIT-ITEM
               PERFORM 3610-PRORATE-APPRAISE
               PERFORM 3620-ASSESS-REVERSAL
               PERFORM 3630-EDIT-MEMO
               PERFORM 3640-SPLIT-LIMIT
               PERFORM 3650-ASSESS-PREFERRED
               PERFORM 3660-ACCUM-REGION
               PERFORM 3670-SELECT-PENALTY-PCT
               PERFORM 3680-SELECT-DISCOUNT
               PERFORM 3690-PRORATE-ANNUAL-FEE
               PERFORM 3700-AGE-EFF
               PERFORM 3710-FLAG-MIN-PAY
               PERFORM 3720-LIMIT-ANNUAL-FEE
               PERFORM 3730-DESCRIBE-REASON
               PERFORM 3740-SPLIT-INTEREST
               PERFORM 3750-DESCRIBE-CHANNEL
               PERFORM 3760-SET-LIEN-POS
               PERFORM 3770-CHECK-NEXT-DUE
               PERFORM 3780-SELECT-DISCOUNT
               PERFORM 3790-DESCRIBE-TRAN
               PERFORM 3800-SELECT-MARGIN
               PERFORM 3810-FLAG-DRAW
               PERFORM 3820-SET-REASON
               PERFORM 3830-ACCRUE-INTEREST
               PERFORM 3840-SELECT-DISCOUNT
               PERFORM 3850-SPLIT-MIN-PAY
               PERFORM 3860-ASSESS-AUTO-RENEW
               PERFORM 3870-PRORATE-ANNUAL-FEE
               PERFORM 3880-DESCRIBE-DRAW-PERIOD
               PERFORM 3890-CHECK-POST
               PERFORM 3900-LIMIT-MIN-PAY
               PERFORM 3910-DESCRIBE-STATUS
               PERFORM 3920-LIMIT-ANNUAL-FEE
               PERFORM 3930-SET-STATUS
               PERFORM 3940-ACCUM-TRAN
               PERFORM 3950-CHECK-OPEN
               PERFORM 3960-SPLIT-APPRAISE
               PERFORM 3970-COUNT-EXCEPT
               PERFORM 3980-COUNT-SEQ
               PERFORM 3990-ACCUM-LIEN-POS
               PERFORM 4000-LIMIT-DRAW
               PERFORM 4010-EDIT-MEMO
               PERFORM 4020-EDIT-REF-NO
               PERFORM 4030-CHECK-CLOSE
               PERFORM 4040-EDIT-CUST-ID
               PERFORM 4050-COUNT-NOTICE
               PERFORM 4060-LIMIT-ITEM
               PERFORM 4070-LIMIT-ANNUAL-FEE
               PERFORM 4080-PRORATE-INTEREST
               PERFORM 4090-SELECT-MARGIN
               PERFORM 4100-SET-TRAN
               PERFORM 4110-ASSESS-REVERSAL
               PERFORM 4120-SET-REGION
               PERFORM 4130-ACCUM-REASON
               PERFORM 4140-SET-LIEN-POS
               PERFORM 4150-SET-CHANNEL
               PERFORM 4160-CHECK-NEXT-DUE
               PERFORM 4170-AGE-EFF
               PERFORM 4180-DESCRIBE-REASON
               PERFORM 4190-FLAG-ITEM
               PERFORM 4200-DESCRIBE-DRAW-PERIOD
               PERFORM 4210-PRORATE-DRAW
               PERFORM 4220-LIMIT-MIN-PAY
               PERFORM 4230-FLAG-MIN-PAY
               PERFORM 4240-ASSESS-PREFERRED
               PERFORM 4250-DESCRIBE-REASON
               PERFORM 4260-ACCUM-CHANNEL
               PERFORM 4270-SPLIT-INTEREST
               PERFORM 4280-SET-TRAN
               PERFORM 4290-COUNT-PAYMENT
               PERFORM 4300-SPLIT-APPRAISE
               PERFORM 4310-COUNT-NOTICE
               PERFORM 4320-DESCRIBE-CHANNEL
               PERFORM 4330-ASSESS-NOTICE-SENT
               PERFORM 4340-ACCUM-STATUS
               PERFORM 4350-COUNT-NOTICE
               PERFORM 4360-ACCUM-REASON
               PERFORM 4370-CHECK-NEXT-REVIEW
               PERFORM 4380-PRORATE-APPRAISE
               PERFORM 4390-CHECK-NEXT-REVIEW
               PERFORM 4400-SET-REGION
               PERFORM 4410-ACCRUE-BALANCE
               PERFORM 4420-ACCUM-LIEN-POS
               PERFORM 4430-SET-REASON
               PERFORM 4440-SELECT-DISCOUNT
               PERFORM 4450-SELECT-MARGIN
               PERFORM 4460-SELECT-PENALTY-PCT
               PERFORM 4470-SPLIT-DRAW
               PERFORM 4480-AGE-EXPIRY
               PERFORM 4490-ACCRUE-PENALTY-PCT
               PERFORM 4500-PRORATE-ANNUAL-FEE
               PERFORM 4510-DESCRIBE-STATUS
               PERFORM 4520-SELECT-MARGIN
               PERFORM 4530-SELECT-DISCOUNT
               PERFORM 4540-AGE-EXPIRY
               PERFORM 4550-ACCUM-REGION
               PERFORM 4560-CHECK-EFF
               PERFORM 4570-SET-TYPE
               PERFORM 4580-ACCUM-CHANNEL
               PERFORM 4590-COUNT-NOTICE
               PERFORM 4600-COUNT-NOTICE
               PERFORM 4610-SELECT-PENALTY-PCT
               PERFORM 4620-LIMIT-LIMIT
               PERFORM 4630-DESCRIBE-DRAW-PERIOD
               PERFORM 4640-AGE-EFF
               PERFORM 4650-ACCUM-TYPE
               PERFORM 4660-COUNT-PAYMENT
               PERFORM 4670-EDIT-ADDR
               PERFORM 4680-SET-DRAW-PERIOD
               PERFORM 4690-PRORATE-APPRAISE
               PERFORM 4700-COUNT-SEQ
               PERFORM 4710-FLAG-INTEREST
               PERFORM 4720-SET-DRAW-PERIOD
               PERFORM 4730-ACCRUE-MIN-PAY
               PERFORM 4740-PRORATE-MIN-PAY
               PERFORM 4750-LIMIT-ANNUAL-FEE
               PERFORM 4760-ACCUM-TRAN
               PERFORM 4770-CHECK-LAST-MAINT
               PERFORM 4780-SET-LIEN-POS
               PERFORM 7000-CALL-HEWAIV
               PERFORM 7010-CALL-HELIM
               PERFORM 7040-CALL-HERISK
               REWRITE HEL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'HELMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM HEL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E668 MEMO REQUIRED
      * E993 DRAW AMT NEGATIVE
      * E424 DRAW AMT OVER 99999
      * E560 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT < 0
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT > 99999.00
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN HELM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELM-DRAW-AMT / 2
           COMPUTE WS-WORK-AMT-2 = HELM-DRAW-AMT - (WS-WORK-AMT * 2)
           COMPUTE HELM-DRAW-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE APPRAISE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN PAY AMT.
      *----------------------------------------------------------------*
       3030-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-MIN-PAY-AMT ROUNDED =
                   HELT-APPRAISE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-MIN-PAY-AMT
           END-IF
           ADD HELM-MIN-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE ANNUAL FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-ANNUAL-FEE-YTD NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW YTD FEE ON THE APPRAISE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3060-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HELM-DRAW-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-DRAW-YTD ROUNDED = HELM-APPRAISE-YTD *
                       HELM-MARGIN / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 12.00 TO HELM-DRAW-YTD
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 3.00 TO HELM-DRAW-YTD
               WHEN OTHER
                   COMPUTE HELM-DRAW-YTD ROUNDED = HELM-APPRAISE-YTD *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-DRAW-YTD > 0 AND HELM-DRAW-YTD < 10.00
               MOVE 10.00 TO HELM-DRAW-YTD
           END-IF
           IF HELM-DRAW-YTD > 50.00
               MOVE 50.00 TO HELM-DRAW-YTD
           END-IF
           ADD HELM-DRAW-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3070-LIMIT-APPRAISE.
           IF HELM-APPRAISE-AMT < 50.00
               MOVE 50.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-AMT > 2500.00
               MOVE 2500.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E288 ADDR REQUIRED
      * E628 ANNUAL FEE AMT NEGATIVE
      * E288 ANNUAL FEE AMT OVER 1000000
      * E715 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-ADDR = SPACES
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT < 0
                   MOVE 'E628' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT > 1000000.00
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE MIN PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-MIN-PAY-AMT NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       3100-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-APPRAISE-AMT ROUNDED =
                   HELM-DRAW-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELT-APPRAISE-AMT
           END-IF
           ADD HELT-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3110-LIMIT-INTEREST.
           IF HELM-INTEREST-YTD < 50.00
               MOVE 50.00 TO HELM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-INTEREST-YTD > 10000.00
               MOVE 10000.00 TO HELM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRAW AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-DRAW-AMT ROUNDED =
                   HELM-MIN-PAY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-DRAW-AMT
           END-IF
           ADD HELM-DRAW-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E084 ADDR REQUIRED
      * E764 ANNUAL FEE AMT NEGATIVE
      * E090 ANNUAL FEE AMT OVER 10000
      * E513 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-ADDR = SPACES
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN HELT-ANNUAL-FEE-AMT < 0
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN HELT-ANNUAL-FEE-AMT > 10000.00
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN HELM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3150-LIMIT-LIMIT.
           IF HELM-LIMIT-AMT < 50.00
               MOVE 50.00 TO HELM-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-LIMIT-AMT > 5000.00
               MOVE 5000.00 TO HELM-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REGION-CD
                   ADD HELM-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 5
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E236 OPERATOR REQUIRED
      * E812 APPRAISE AMT NEGATIVE
      * E638 APPRAISE AMT OVER 250000
      * E858 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-OPERATOR = SPACES
                   MOVE 'E236' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT < 0
                   MOVE 'E812' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT > 250000.00
                   MOVE 'E638' TO WS-REJECT-CODE
               WHEN HELM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE APPRAISE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3190-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELT-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE HELT-LIMIT-AMT ROUNDED = HELM-APPRAISE-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-TYPE-CD = 'CD'
                   MOVE 20.00 TO HELT-LIMIT-AMT
               WHEN HELM-TYPE-CD = 'JW'
                   MOVE 7.00 TO HELT-LIMIT-AMT
               WHEN OTHER
                   COMPUTE HELT-LIMIT-AMT ROUNDED = HELM-APPRAISE-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELT-LIMIT-AMT > 0 AND HELT-LIMIT-AMT < 2.00
               MOVE 2.00 TO HELT-LIMIT-AMT
           END-IF
           IF HELT-LIMIT-AMT > 250.00
               MOVE 250.00 TO HELT-LIMIT-AMT
           END-IF
           ADD HELT-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE BALANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       3200-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-LIMIT-YTD ROUNDED =
                   HELM-BALANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-LIMIT-YTD
           END-IF
           ADD HELM-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = HELM-LIMIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = HELM-LIMIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE HELM-DRAW-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E481 NAME REQUIRED
      * E241 INTEREST YTD NEGATIVE
      * E411 INTEREST YTD OVER 50000
      * E189 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-NAME = SPACES
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-YTD < 0
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-YTD > 50000.00
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN HELM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DRAW AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-DRAW.
           IF HELT-DRAW-AMT < 10.00
               MOVE 10.00 TO HELT-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-DRAW-AMT > 10000.00
               MOVE 10000.00 TO HELT-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT < 0.00
               MOVE 0.00 TO HELT-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-ANNUAL-FEE-AMT > 1000.00
               MOVE 1000.00 TO HELT-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-TRAN-CD
                   ADD HELM-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELM-APPRAISE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = HELM-APPRAISE-YTD - (WS-WORK-AMT * 4)
           COMPUTE HELM-MIN-PAY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN LAST MAINT DATE IS PAST
      * PY -> FZ WHEN LAST MAINT DATE IS PAST
      * FZ -> RC WHEN LAST MAINT DATE IS PAST
      * RC -> AF WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 6
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE YTD FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3320-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELM-ANNUAL-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                       HELT-ANNUAL-FEE-AMT * HELM-MARGIN / 200
               WHEN HELM-REGION-CD = 'BSD'
                   MOVE 5.00 TO HELM-ANNUAL-FEE-YTD
               WHEN HELM-REGION-CD = 'HKW'
                   MOVE 5.00 TO HELM-ANNUAL-FEE-YTD
               WHEN OTHER
                   COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                       HELT-ANNUAL-FEE-AMT * HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-ANNUAL-FEE-YTD > 0 AND HELM-ANNUAL-FEE-YTD < 5.00
               MOVE 5.00 TO HELM-ANNUAL-FEE-YTD
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 75.00
               MOVE 75.00 TO HELM-ANNUAL-FEE-YTD
           END-IF
           ADD HELM-ANNUAL-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3330-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-INTEREST-YTD ROUNDED =
                   HELM-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW FEE ON THE LIMIT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3340-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HELT-DRAW-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE HELT-DRAW-AMT ROUNDED = HELM-LIMIT-AMT *
                       HELM-MARGIN / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 7.00 TO HELT-DRAW-AMT
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 12.00 TO HELT-DRAW-AMT
               WHEN OTHER
                   COMPUTE HELT-DRAW-AMT ROUNDED = HELM-LIMIT-AMT *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELT-DRAW-AMT > 0 AND HELT-DRAW-AMT < 10.00
               MOVE 10.00 TO HELT-DRAW-AMT
           END-IF
           IF HELT-DRAW-AMT > 100.00
               MOVE 100.00 TO HELT-DRAW-AMT
           END-IF
           ADD HELT-DRAW-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF HELT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3370-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   HELM-ANNUAL-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3380-FLAG-LIMIT.
           IF HELM-LIMIT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3390-FLAG-MIN-PAY.
           IF HELM-MIN-PAY-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-MIN-PAY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN PAY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       3400-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-LIMIT-YTD ROUNDED =
                   HELM-MIN-PAY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-LIMIT-YTD
           END-IF
           ADD HELM-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3420-FLAG-INTEREST.
           IF HELM-INTEREST-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELT-ANNUAL-FEE-AMT * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELM-APPRAISE-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELM-DRAW-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HELM-DRAW-YTD - (WS-WORK-AMT * 3)
           COMPUTE HELM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-LIMIT.
           IF HELM-LIMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-LIMIT-YTD * HELM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HELM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-AMT * HELM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HELM-BALANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-INTEREST.
           IF HELM-INTEREST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE LIMIT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3530-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-AMT ROUNDED = HELM-LIMIT-YTD *
                       HELM-MARGIN / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 3.00 TO HELM-BALANCE-AMT
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 7.00 TO HELM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE HELM-BALANCE-AMT ROUNDED = HELM-LIMIT-YTD *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-BALANCE-AMT > 0 AND HELM-BALANCE-AMT < 5.00
               MOVE 5.00 TO HELM-BALANCE-AMT
           END-IF
           IF HELM-BALANCE-AMT > 150.00
               MOVE 150.00 TO HELM-BALANCE-AMT
           END-IF
           ADD HELM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E704 REF NO REQUIRED
      * E792 WORK AMT 2 NEGATIVE
      * E826 WORK AMT 2 OVER 5000
      * E091 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN HELT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HELT-ANNUAL-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELT-ANNUAL-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 6
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY DRAW PERIOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-DRAW-PERIOD-CD
                   ADD HELM-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0 AND HELM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3600-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-AMT ROUNDED =
                   HELM-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3620-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = WS-WORK-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 7.00 TO HELM-INTEREST-YTD
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 5.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = WS-WORK-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 10.00
               MOVE 10.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 75.00
               MOVE 75.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E625 MEMO REQUIRED
      * E786 MIN PAY AMT NEGATIVE
      * E955 MIN PAY AMT OVER 1000000
      * E241 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT < 0
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT > 1000000.00
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN HELM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = HELM-LIMIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = HELM-LIMIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE HELM-DRAW-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE BALANCE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3650-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-AMT ROUNDED = HELM-BALANCE-AMT
                       * HELM-PENALTY-PCT / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 20.00 TO HELM-APPRAISE-AMT
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 3.00 TO HELM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-AMT ROUNDED = HELM-BALANCE-AMT
                       * HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-APPRAISE-AMT > 0 AND HELM-APPRAISE-AMT < 1.00
               MOVE 1.00 TO HELM-APPRAISE-AMT
           END-IF
           IF HELM-APPRAISE-AMT > 25.00
               MOVE 25.00 TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-ANNUAL-FEE-AMT NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELT-ANNUAL-FEE-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       3690-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-AMT
           END-IF
           ADD HELM-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-EFF-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN PAY AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3710-FLAG-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-MIN-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3720-LIMIT-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT < 50.00
               MOVE 50.00 TO HELT-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-ANNUAL-FEE-AMT > 500.00
               MOVE 500.00 TO HELT-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3740-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HELM-INTEREST-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HELM-INTEREST-YTD - (WS-WORK-AMT * 3)
           COMPUTE HELT-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN POST DATE IS PAST
      * PVA -> SFC WHEN POST DATE IS PAST
      * SFC -> AXX WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-DRAW-AMT NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-APPRAISE-YTD NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRAW AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3810-FLAG-DRAW.
           IF HELM-DRAW-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-DRAW-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN EFF DATE IS PAST
      * NWS -> ZJM WHEN EFF DATE IS PAST
      * ZJM -> KST WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KST' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-INTEREST.
           IF HELM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-INTEREST-YTD * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-DRAW-AMT NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN PAY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-MIN-PAY.
           COMPUTE WS-WORK-AMT = HELM-MIN-PAY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HELM-MIN-PAY-YTD - (WS-WORK-AMT * 3)
           COMPUTE HELM-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE ANNUAL FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3860-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-AMT ROUNDED =
                       HELM-ANNUAL-FEE-YTD * HELM-DISCOUNT / 200
               WHEN HELM-REGION-CD = 'BSD'
                   MOVE 10.00 TO HELM-APPRAISE-AMT
               WHEN HELM-REGION-CD = 'HKW'
                   MOVE 7.00 TO HELM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-AMT ROUNDED =
                       HELM-ANNUAL-FEE-YTD * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-APPRAISE-AMT > 0 AND HELM-APPRAISE-AMT < 1.00
               MOVE 1.00 TO HELM-APPRAISE-AMT
           END-IF
           IF HELM-APPRAISE-AMT > 250.00
               MOVE 250.00 TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       3870-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                   HELM-DRAW-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-AMT
           END-IF
           ADD HELM-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF HELT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-AMT < 100.00
               MOVE 100.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-AMT > 2500.00
               MOVE 2500.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT < 100.00
               MOVE 100.00 TO HELT-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-ANNUAL-FEE-AMT > 50000.00
               MOVE 50000.00 TO HELT-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * GR -> JC WHEN POST DATE IS PAST
      * JC -> KH WHEN POST DATE IS PAST
      * KH -> KN WHEN POST DATE IS PAST
      * KN -> PC WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KH' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KH'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KN' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KN'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-TRAN-CD
                   ADD HELM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HELM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELM-APPRAISE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = HELM-APPRAISE-AMT - (WS-WORK-AMT * 2)
           COMPUTE HELM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 5
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DRAW AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4000-LIMIT-DRAW.
           IF HELT-DRAW-AMT < 25.00
               MOVE 25.00 TO HELT-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-DRAW-AMT > 500.00
               MOVE 500.00 TO HELT-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E725 MEMO REQUIRED
      * E072 INTEREST YTD NEGATIVE
      * E702 INTEREST YTD OVER 25000
      * E505 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-YTD < 0
                   MOVE 'E072' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-YTD > 25000.00
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN HELT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E833 REF NO REQUIRED
      * E826 DRAW AMT NEGATIVE
      * E556 DRAW AMT OVER 10000
      * E522 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN HELT-DRAW-AMT < 0
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN HELT-DRAW-AMT > 10000.00
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E522' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HELM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E007 CUST ID REQUIRED
      * E852 INTEREST YTD NEGATIVE
      * E604 INTEREST YTD OVER 1000000
      * E056 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CUST-ID = SPACES
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-YTD < 0
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-YTD > 1000000.00
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4070-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 0.00
               MOVE 0.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 50000.00
               MOVE 50000.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4080-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   HELM-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELM-LIMIT-YTD NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN LAST MAINT DATE IS PAST
      * PY -> FZ WHEN LAST MAINT DATE IS PAST
      * FZ -> RC WHEN LAST MAINT DATE IS PAST
      * RC -> AF WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4110-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO HELT-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                       HELM-ANNUAL-FEE-AMT * HELM-DISCOUNT / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 12.00 TO HELT-ANNUAL-FEE-AMT
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 12.00 TO HELT-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                       HELM-ANNUAL-FEE-AMT * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELT-ANNUAL-FEE-AMT > 0 AND HELT-ANNUAL-FEE-AMT < 5.00
               MOVE 5.00 TO HELT-ANNUAL-FEE-AMT
           END-IF
           IF HELT-ANNUAL-FEE-AMT > 100.00
               MOVE 100.00 TO HELT-ANNUAL-FEE-AMT
           END-IF
           ADD HELT-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN EFF DATE IS PAST
      * HKW -> HPB WHEN EFF DATE IS PAST
      * HPB -> ULJ WHEN EFF DATE IS PAST
      * ULJ -> BSD WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REASON-CD
                   ADD HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN EXPIRY DATE IS PAST
      * PVA -> SFC WHEN EXPIRY DATE IS PAST
      * SFC -> AXX WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4190-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRAW YTD.
      *----------------------------------------------------------------*
       4210-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-DRAW-YTD ROUNDED =
                   HELM-DRAW-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-DRAW-YTD
           END-IF
           ADD HELM-DRAW-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MIN PAY YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-YTD < 10.00
               MOVE 10.00 TO HELM-MIN-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-YTD > 10000.00
               MOVE 10000.00 TO HELM-MIN-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4230-FLAG-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-MIN-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4240-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = WS-WORK-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 20.00 TO HELM-INTEREST-YTD
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 15.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = WS-WORK-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 10.00
               MOVE 10.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 250.00
               MOVE 250.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-CHANNEL-CD
                   ADD HELM-LIMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-LIMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4270-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HELM-INTEREST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELM-INTEREST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE HELT-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN OPEN DATE IS PAST
      * PY -> FZ WHEN OPEN DATE IS PAST
      * FZ -> RC WHEN OPEN DATE IS PAST
      * RC -> AF WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4280-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4300-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELT-APPRAISE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = HELT-APPRAISE-AMT - (WS-WORK-AMT * 3)
           COMPUTE HELM-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4310-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4320-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE ANNUAL FEE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4330-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-YTD ROUNDED =
                       HELM-ANNUAL-FEE-YTD * HELM-MARGIN / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 15.00 TO HELM-BALANCE-YTD
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 15.00 TO HELM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE HELM-BALANCE-YTD ROUNDED =
                       HELM-ANNUAL-FEE-YTD * HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-BALANCE-YTD > 0 AND HELM-BALANCE-YTD < 2.00
               MOVE 2.00 TO HELM-BALANCE-YTD
           END-IF
           IF HELM-BALANCE-YTD > 35.00
               MOVE 35.00 TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-STATUS-CD
                   ADD HELM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REASON-CD
                   ADD HELM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4370-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       4380-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN EFF DATE IS PAST
      * HKW -> HPB WHEN EFF DATE IS PAST
      * HPB -> ULJ WHEN EFF DATE IS PAST
      * ULJ -> BSD WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4410-ACCRUE-BALANCE.
           IF HELM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-BALANCE-YTD * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HELM-MIN-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4420-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELM-LIMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-LIMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN MATURITY DATE IS PAST
      * NWS -> ZJM WHEN MATURITY DATE IS PAST
      * ZJM -> KST WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4430-SET-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KST' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE MIN PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4440-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELM-MIN-PAY-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4450-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-INTEREST-AMT NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4460-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELM-DRAW-AMT / 3
           COMPUTE WS-WORK-AMT-2 = HELM-DRAW-AMT - (WS-WORK-AMT * 3)
           COMPUTE HELM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4480-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4490-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELM-ANNUAL-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE APPRAISE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ANNUAL FEE YTD.
      *----------------------------------------------------------------*
       4500-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                   HELM-APPRAISE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-YTD
           END-IF
           ADD HELM-ANNUAL-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4510-DESCRIBE-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE DRAW YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4520-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELM-DRAW-YTD NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4530-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELT-DRAW-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4540-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4550-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REGION-CD
                   ADD HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4560-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF HELT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN LAST MAINT DATE IS PAST
      * JW -> KV WHEN LAST MAINT DATE IS PAST
      * KV -> WS WHEN LAST MAINT DATE IS PAST
      * WS -> XL WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4570-SET-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JW' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KV' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WS' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XL' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4580-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-CHANNEL-CD
                   ADD HELT-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4590-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4600-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE MIN PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4610-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-MIN-PAY-AMT NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4620-LIMIT-LIMIT.
           IF HELT-LIMIT-AMT < 0.00
               MOVE 0.00 TO HELT-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-LIMIT-AMT > 2500.00
               MOVE 2500.00 TO HELT-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4630-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4640-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4650-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-TYPE-CD
                   ADD HELT-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4660-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E253 ADDR REQUIRED
      * E418 DRAW AMT NEGATIVE
      * E022 DRAW AMT OVER 99999
      * E730 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4670-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-ADDR = SPACES
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN HELT-DRAW-AMT < 0
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN HELT-DRAW-AMT > 99999.00
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN MATURITY DATE IS PAST
      * JSU -> KXA WHEN MATURITY DATE IS PAST
      * KXA -> VMR WHEN MATURITY DATE IS PAST
      * VMR -> WLJ WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4680-SET-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MIN PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE YTD.
      *----------------------------------------------------------------*
       4690-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-YTD ROUNDED =
                   HELM-MIN-PAY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-APPRAISE-YTD
           END-IF
           ADD HELM-APPRAISE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4700-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 3
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4710-FLAG-INTEREST.
           IF HELM-INTEREST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN NEXT REVIEW DATE IS PAST
      * JSU -> KXA WHEN NEXT REVIEW DATE IS PAST
      * KXA -> VMR WHEN NEXT REVIEW DATE IS PAST
      * VMR -> WLJ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4720-SET-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4730-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-YTD * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN PAY YTD.
      *----------------------------------------------------------------*
       4740-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-MIN-PAY-YTD ROUNDED =
                   HELT-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-MIN-PAY-YTD
           END-IF
           ADD HELM-MIN-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4750-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 100.00
               MOVE 100.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 2500.00
               MOVE 2500.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4760-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-TRAN-CD
                   ADD HELM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4770-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN CLOSE DATE IS PAST
      * PVA -> SFC WHEN CLOSE DATE IS PAST
      * SFC -> AXX WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4780-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE HEL FIELDS TO HEWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-HEWAIV.
           INITIALIZE HEWAIV-PARMS
           MOVE HELM-BALANCE-AMT
             TO HEWAIV-BALANCE-AMT
           MOVE HELM-LIMIT-YTD
             TO HEWAIV-LIMIT-YTD
           MOVE HELM-DRAW-AMT
             TO HEWAIV-DRAW-AMT
           MOVE HELM-LIMIT-AMT
             TO HEWAIV-LIMIT-AMT
           MOVE HELM-INTEREST-AMT
             TO HEWAIV-INTEREST-AMT
           MOVE HELM-ANNUAL-FEE-AMT
             TO HEWAIV-ANNUAL-FEE-AMT
           MOVE HELM-LAST-MAINT-DATE
             TO HEWAIV-LAST-MAINT-DATE
           MOVE HELM-LAST-PAY-DATE
             TO HEWAIV-LAST-PAY-DATE
           MOVE HELM-EFFECTIVE-DATE
             TO HEWAIV-EFFECTIVE-DATE
           MOVE HELM-MATURITY-DATE
             TO HEWAIV-MATURITY-DATE
           MOVE HELM-LIEN-POS-CD
             TO HEWAIV-LIEN-POS-CD
           MOVE HELM-REGION-CD
             TO HEWAIV-REGION-CD
           MOVE HELM-DRAW-PERIOD-CD
             TO HEWAIV-DRAW-PERIOD-CD
           MOVE HELM-REASON-CD
             TO HEWAIV-REASON-CD
           MOVE HELM-SUSPEND-FLAG
             TO HEWAIV-SUSPEND-FLAG
           MOVE HELM-HOLD-FLAG
             TO HEWAIV-HOLD-FLAG
           MOVE HELM-PREFERRED-FLAG
             TO HEWAIV-PREFERRED-FLAG
           MOVE HELM-EXCEPT-CNT
             TO HEWAIV-EXCEPT-CNT
           MOVE HELM-NOTICE-CNT
             TO HEWAIV-NOTICE-CNT
           MOVE HELM-MARGIN
             TO HEWAIV-MARGIN
           MOVE HELM-PENALTY-PCT
             TO HEWAIV-PENALTY-PCT
           MOVE HELM-DESC
             TO HEWAIV-DESC
           MOVE HELM-CUST-ID
             TO HEWAIV-CUST-ID
           CALL 'HEWAIV' USING HEWAIV-PARMS
           EVALUATE HEWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE HEWAIV-LIMIT-YTD
                     TO HELM-LIMIT-YTD
                   MOVE HEWAIV-SUSPEND-FLAG
                     TO HELM-SUSPEND-FLAG
                   MOVE HEWAIV-PREFERRED-FLAG
                     TO HELM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-HEWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE HEL FIELDS TO HELIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-HELIM.
           INITIALIZE HELIM-PARMS
           MOVE HELM-BALANCE-YTD
             TO HELIM-BALANCE-YTD
           MOVE HELM-APPRAISE-YTD
             TO HELIM-APPRAISE-YTD
           MOVE HELM-ANNUAL-FEE-AMT
             TO HELIM-ANNUAL-FEE-AMT
           MOVE HELM-APPRAISE-AMT
             TO HELIM-APPRAISE-AMT
           MOVE HELM-INTEREST-YTD
             TO HELIM-INTEREST-YTD
           MOVE HELM-MIN-PAY-AMT
             TO HELIM-MIN-PAY-AMT
           MOVE HELM-EFFECTIVE-DATE
             TO HELIM-EFFECTIVE-DATE
           MOVE HELM-LAST-PAY-DATE
             TO HELIM-LAST-PAY-DATE
           MOVE HELM-NEXT-DUE-DATE
             TO HELIM-NEXT-DUE-DATE
           MOVE HELM-MATURITY-DATE
             TO HELIM-MATURITY-DATE
           MOVE HELM-DRAW-PERIOD-CD
             TO HELIM-DRAW-PERIOD-CD
           MOVE HELM-STATUS-CD
             TO HELIM-STATUS-CD
           MOVE HELM-TYPE-CD
             TO HELIM-TYPE-CD
           MOVE HELM-LIEN-POS-CD
             TO HELIM-LIEN-POS-CD
           MOVE HELM-AUTO-RENEW-FLAG
             TO HELIM-AUTO-RENEW-FLAG
           MOVE HELM-OVERRIDE-FLAG
             TO HELIM-OVERRIDE-FLAG
           MOVE HELM-HOLD-FLAG
             TO HELIM-HOLD-FLAG
           MOVE HELM-PAYMENT-CNT
             TO HELIM-PAYMENT-CNT
           MOVE HELM-EXCEPT-CNT
             TO HELIM-EXCEPT-CNT
           MOVE HELM-PENALTY-PCT
             TO HELIM-PENALTY-PCT
           MOVE HELM-DISCOUNT
             TO HELIM-DISCOUNT
           MOVE HELM-REF-NO
             TO HELIM-REF-NO
           MOVE HELM-DESC
             TO HELIM-DESC
           CALL 'HELIM' USING HELIM-PARMS
           EVALUATE HELIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE HELIM-APPRAISE-YTD
                     TO HELM-APPRAISE-YTD
                   MOVE HELIM-APPRAISE-AMT
                     TO HELM-APPRAISE-AMT
                   MOVE HELIM-INTEREST-YTD
                     TO HELM-INTEREST-YTD
                   MOVE HELIM-AUTO-RENEW-FLAG
                     TO HELM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-HELIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE HEL FIELDS TO HERISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-HERISK.
           INITIALIZE HERISK-PARMS
           MOVE HELM-INTEREST-AMT
             TO HERISK-INTEREST-AMT
           MOVE HELM-MIN-PAY-AMT
             TO HERISK-MIN-PAY-AMT
           MOVE HELM-BALANCE-YTD
             TO HERISK-BALANCE-YTD
           MOVE HELM-ANNUAL-FEE-AMT
             TO HERISK-ANNUAL-FEE-AMT
           MOVE HELM-MIN-PAY-YTD
             TO HERISK-MIN-PAY-YTD
           MOVE HELM-APPRAISE-AMT
             TO HERISK-APPRAISE-AMT
           MOVE HELM-EFFECTIVE-DATE
             TO HERISK-EFFECTIVE-DATE
           MOVE HELM-LAST-MAINT-DATE
             TO HERISK-LAST-MAINT-DATE
           MOVE HELM-OPEN-DATE
             TO HERISK-OPEN-DATE
           MOVE HELM-MATURITY-DATE
             TO HERISK-MATURITY-DATE
           MOVE HELM-REASON-CD
             TO HERISK-REASON-CD
           MOVE HELM-TYPE-CD
             TO HERISK-TYPE-CD
           MOVE HELM-STATUS-CD
             TO HERISK-STATUS-CD
           MOVE HELM-DRAW-PERIOD-CD
             TO HERISK-DRAW-PERIOD-CD
           MOVE HELM-HOLD-FLAG
             TO HERISK-HOLD-FLAG
           MOVE HELM-PREFERRED-FLAG
             TO HERISK-PREFERRED-FLAG
           MOVE HELM-SUSPEND-FLAG
             TO HERISK-SUSPEND-FLAG
           MOVE HELM-RETRY-CNT
             TO HERISK-RETRY-CNT
           MOVE HELM-PAYMENT-CNT
             TO HERISK-PAYMENT-CNT
           MOVE HELM-PENALTY-PCT
             TO HERISK-PENALTY-PCT
           MOVE HELM-MARGIN
             TO HERISK-MARGIN
           MOVE HELM-CONTACT
             TO HERISK-CONTACT
           MOVE HELM-DESC
             TO HERISK-DESC
           CALL 'HERISK' USING HERISK-PARMS
           EVALUATE HERISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE HERISK-INTEREST-AMT
                     TO HELM-INTEREST-AMT
                   MOVE HERISK-MIN-PAY-AMT
                     TO HELM-MIN-PAY-AMT
                   MOVE HERISK-MIN-PAY-YTD
                     TO HELM-MIN-PAY-YTD
                   MOVE HERISK-APPRAISE-AMT
                     TO HELM-APPRAISE-AMT
                   MOVE HERISK-HOLD-FLAG
                     TO HELM-HOLD-FLAG
                   MOVE HERISK-PREFERRED-FLAG
                     TO HELM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-HERISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE HELT-KEY TO RD-KEY
           MOVE HELT-TRAN-CD TO RD-CODE
           MOVE HELT-DRAW-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE HELT-KEY TO RD-KEY
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
           DISPLAY 'HELEXCP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'HELEXCP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
