      *================================================================*
      * PROGRAM  : HELACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : HOME EQUITY LINES - DAILY ACCRUAL. READS THE HEL    *
      *            TRANSACTIONS, UPDATES THE HEL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOME EQUITY LINE DRAWS, BILLING,*
      *            RATE CHANGE AND FREEZE                              *
      * INPUT    : HELMST (KSDS), HELTRN, BANKCTL                      *
      * OUTPUT   : HELOUT, HELACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, HEFEE, HESTAT, HELIM    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-25 EWS  ORIGINAL                                       *
      * 1991-08-24 BHS  MIN-PAY EXCEPTIONS TO OPS QUEUE REQ2742        *
      * 1993-09-03 TWH  APPRAISE CUTOFF MOVED CHG7175                  *
      * 1993-09-25 JLM  BALANCE CALCULATION CORRECTED CHG5911          *
      * 1995-12-08 SKN  APPRAISE STATUS CODES ADDED PRB3111            *
      * 2002-02-25 PQR  ANNUAL-FEE ROUNDING - HALF UP PRB5347          *
      * 2003-01-20 BHS  LIMIT TABLE EXPANDED PRB4735                   *
      * 2005-08-20 RJT  INTEREST CALCULATION CORRECTED PRB5880         *
      * 2006-10-24 DPS  DRAW NOTICE TEXT PER LEGAL REQ6088             *
      * 2007-01-08 EWS  INTEREST LIMIT REVISED INC8842                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELACCR.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "HELACRPT"
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
                                           VALUE 25.000000.
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
      * WS-DRAW-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRAW-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000611000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000118000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000452000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000296500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000849000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000225000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000832000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019250000000661500'.
       01  WS-DRAW-SCHED-TBL REDEFINES WS-DRAW-SCHED-VALUES.
           05  WS-DRAW-SCHED-ROW OCCURS 8 TIMES.
               10  WS-DRAW-SCHED-LOW       PIC 9(09)V99.
               10  WS-DRAW-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LIMIT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LIMIT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000516000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000204500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000068000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000509500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000596000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016550000000742000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016650000000499000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016700000000139000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016750000000626500'.
       01  WS-LIMIT-SCHED-TBL REDEFINES WS-LIMIT-SCHED-VALUES.
           05  WS-LIMIT-SCHED-ROW OCCURS 9 TIMES.
               10  WS-LIMIT-SCHED-LOW      PIC 9(09)V99.
               10  WS-LIMIT-SCHED-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BALANCE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BALANCE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000624000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000014000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000847500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000323500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000289500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007100000000107500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007350000000549000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017350000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027350000000115000'.
       01  WS-BALANCE-BAND-TBL REDEFINES WS-BALANCE-BAND-VALUES.
           05  WS-BALANCE-BAND-ROW OCCURS 9 TIMES.
               10  WS-BALANCE-BAND-LOW     PIC 9(09)V99.
               10  WS-BALANCE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-APPRAISE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-APPRAISE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000267500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000270500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000045000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000720000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000830500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022100000000371500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032100000000270500'.
       01  WS-APPRAISE-BAND-TBL REDEFINES WS-APPRAISE-BAND-VALUES.
           05  WS-APPRAISE-BAND-ROW OCCURS 7 TIMES.
               10  WS-APPRAISE-BAND-LOW    PIC 9(09)V99.
               10  WS-APPRAISE-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  HELACCR  HOME EQUITY LINES'.
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
       COPY HEFEEP.
       COPY HESTATP.
       COPY HELIMP.

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
               PERFORM 3000-ASSESS-PREFERRED
               PERFORM 3010-SET-STATUS
               PERFORM 3020-EDIT-DESC
               PERFORM 3030-SPLIT-BALANCE
               PERFORM 3040-DESCRIBE-REASON
               PERFORM 3050-CHECK-MATURITY
               PERFORM 3060-ASSESS-PREFERRED
               PERFORM 3070-DESCRIBE-REASON
               PERFORM 3080-PRORATE-LIMIT
               PERFORM 3090-LIMIT-APPRAISE
               PERFORM 3100-CHECK-EFF
               PERFORM 3110-EDIT-DESC
               PERFORM 3120-SELECT-DISCOUNT
               PERFORM 3130-CHECK-LAST-MAINT
               PERFORM 3140-LIMIT-LIMIT
               PERFORM 3150-AGE-MATURITY
               PERFORM 3160-ACCUM-DRAW-PERIOD
               PERFORM 3170-COUNT-NOTICE
               PERFORM 3180-SELECT-DISCOUNT
               PERFORM 3190-CHECK-MATURITY
               PERFORM 3200-ACCUM-LIEN-POS
               PERFORM 3210-EDIT-DESC
               PERFORM 3220-SELECT-DISCOUNT
               PERFORM 3230-ACCRUE-MIN-PAY
               PERFORM 3240-ASSESS-OVERRIDE
               PERFORM 3250-CHECK-EFF
               PERFORM 3260-AGE-POST
               PERFORM 3270-ASSESS-PREFERRED
               PERFORM 3280-COUNT-PAYMENT
               PERFORM 3290-LIMIT-LIMIT
               PERFORM 3300-SPLIT-DRAW
               PERFORM 3310-ACCUM-CHANNEL
               PERFORM 3320-CHECK-MATURITY
               PERFORM 3330-AGE-EFF
               PERFORM 3340-ACCRUE-DRAW
               PERFORM 3350-EDIT-REJECT
               PERFORM 3360-ASSESS-PREFERRED
               PERFORM 3370-LIMIT-LIMIT
               PERFORM 3380-SELECT-MARGIN
               PERFORM 3390-CHECK-OPEN
               PERFORM 3400-SELECT-MARGIN
               PERFORM 3410-DESCRIBE-CHANNEL
               PERFORM 3420-LIMIT-APPRAISE
               PERFORM 3430-FLAG-INTEREST
               PERFORM 3440-EDIT-MEMO
               PERFORM 3450-FLAG-APPRAISE
               PERFORM 3460-DESCRIBE-DRAW-PERIOD
               PERFORM 3470-FLAG-APPRAISE
               PERFORM 3480-PRORATE-ANNUAL-FEE
               PERFORM 3490-AGE-NEXT-DUE
               PERFORM 3500-ACCRUE-DRAW
               PERFORM 3510-ACCUM-DRAW-PERIOD
               PERFORM 3520-PRORATE-LIMIT
               PERFORM 3530-FLAG-APPRAISE
               PERFORM 3540-EDIT-CUST-ID
               PERFORM 3550-SET-TRAN
               PERFORM 3560-FLAG-LIMIT
               PERFORM 3570-EDIT-REF-NO
               PERFORM 3580-FLAG-BALANCE
               PERFORM 3590-PRORATE-APPRAISE
               PERFORM 3600-EDIT-MEMO
               PERFORM 3610-FLAG-INTEREST
               PERFORM 3620-ASSESS-NOTICE-SENT
               PERFORM 3630-COUNT-NOTICE
               PERFORM 3640-ACCRUE-LIMIT
               PERFORM 3650-SELECT-PENALTY-PCT
               PERFORM 3660-DESCRIBE-TRAN
               PERFORM 3670-CHECK-NEXT-REVIEW
               PERFORM 3680-SELECT-MARGIN
               PERFORM 3690-COUNT-SEQ
               PERFORM 3700-SELECT-MARGIN
               PERFORM 3710-PRORATE-ANNUAL-FEE
               PERFORM 3720-FLAG-LIMIT
               PERFORM 3730-DESCRIBE-LIEN-POS
               PERFORM 3740-EDIT-CONTACT
               PERFORM 3750-SELECT-DISCOUNT
               PERFORM 3760-PRORATE-LIMIT
               PERFORM 3770-ACCRUE-MIN-PAY
               PERFORM 3780-PRORATE-LIMIT
               PERFORM 3790-EDIT-CONTACT
               PERFORM 3800-SET-TYPE
               PERFORM 3810-COUNT-EXCEPT
               PERFORM 3820-FLAG-APPRAISE
               PERFORM 3830-COUNT-NOTICE
               PERFORM 3840-COUNT-PAYMENT
               PERFORM 3850-AGE-MATURITY
               PERFORM 3860-AGE-OPEN
               PERFORM 3870-ACCRUE-APPRAISE
               PERFORM 3880-DESCRIBE-DRAW-PERIOD
               PERFORM 3890-LIMIT-ANNUAL-FEE
               PERFORM 3900-ASSESS-PREFERRED
               PERFORM 3910-FLAG-APPRAISE
               PERFORM 3920-SET-REASON
               PERFORM 3930-AGE-EFF
               PERFORM 3940-AGE-LAST-MAINT
               PERFORM 3950-SELECT-PENALTY-PCT
               PERFORM 3960-ASSESS-AUTO-RENEW
               PERFORM 3970-FLAG-LIMIT
               PERFORM 3980-PRORATE-BALANCE
               PERFORM 3990-FLAG-DRAW
               PERFORM 4000-FLAG-INTEREST
               PERFORM 4010-LIMIT-INTEREST
               PERFORM 4020-FLAG-BALANCE
               PERFORM 4030-CHECK-MATURITY
               PERFORM 4040-SET-STATUS
               PERFORM 4050-ASSESS-AUTO-RENEW
               PERFORM 4060-SET-REASON
               PERFORM 4070-SPLIT-APPRAISE
               PERFORM 4080-COUNT-PAYMENT
               PERFORM 4090-DESCRIBE-DRAW-PERIOD
               PERFORM 4100-LIMIT-MIN-PAY
               PERFORM 4110-FLAG-LIMIT
               PERFORM 4120-ACCUM-TRAN
               PERFORM 4130-ASSESS-HOLD
               PERFORM 4140-EDIT-REF-NO
               PERFORM 4150-EDIT-CONTACT
               PERFORM 4160-FLAG-BALANCE
               PERFORM 4170-ASSESS-SUSPEND
               PERFORM 4180-PRORATE-BALANCE
               PERFORM 4190-ASSESS-PREFERRED
               PERFORM 4200-EDIT-DESC
               PERFORM 4210-ACCUM-STATUS
               PERFORM 4220-ACCUM-REGION
               PERFORM 4230-ACCUM-REGION
               PERFORM 4240-FLAG-DRAW
               PERFORM 4250-CHECK-CLOSE
               PERFORM 4260-SELECT-DISCOUNT
               PERFORM 4270-DESCRIBE-LIEN-POS
               PERFORM 4280-SET-CHANNEL
               PERFORM 4290-AGE-LAST-PAY
               PERFORM 4300-COUNT-SEQ
               PERFORM 4310-FLAG-ITEM
               PERFORM 4320-SET-LIEN-POS
               PERFORM 4330-EDIT-MEMO
               PERFORM 4340-COUNT-RETRY
               PERFORM 4350-DESCRIBE-LIEN-POS
               PERFORM 4360-ACCRUE-DRAW
               PERFORM 4370-EDIT-REF-NO
               PERFORM 4380-ASSESS-HOLD
               PERFORM 4390-PRORATE-MIN-PAY
               PERFORM 4400-EDIT-MEMO
               PERFORM 4410-SELECT-MARGIN
               PERFORM 4420-DESCRIBE-CHANNEL
               PERFORM 4430-FLAG-BALANCE
               PERFORM 4440-FLAG-DRAW
               PERFORM 4450-DESCRIBE-LIEN-POS
               PERFORM 4460-CHECK-OPEN
               PERFORM 4470-SPLIT-ANNUAL-FEE
               PERFORM 4480-EDIT-REF-NO
               PERFORM 4490-LIMIT-APPRAISE
               PERFORM 4500-ACCUM-CHANNEL
               PERFORM 4510-SET-TYPE
               PERFORM 4520-ASSESS-AUTO-RENEW
               PERFORM 4530-DESCRIBE-LIEN-POS
               PERFORM 4540-ASSESS-PREFERRED
               PERFORM 4550-FLAG-BALANCE
               PERFORM 4560-SPLIT-DRAW
               PERFORM 4570-EDIT-CUST-ID
               PERFORM 4580-SPLIT-BALANCE
               PERFORM 4590-SET-STATUS
               PERFORM 4600-SELECT-DISCOUNT
               PERFORM 4610-FLAG-LIMIT
               PERFORM 7030-CALL-HEFEE
               PERFORM 7020-CALL-HESTAT
               PERFORM 7010-CALL-HELIM
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
      * ASSESS THE BALANCE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3000-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-YTD ROUNDED = WS-WORK-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 15.00 TO HELM-BALANCE-YTD
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 20.00 TO HELM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE HELM-BALANCE-YTD ROUNDED = WS-WORK-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-BALANCE-YTD > 0 AND HELM-BALANCE-YTD < 2.00
               MOVE 2.00 TO HELM-BALANCE-YTD
           END-IF
           IF HELM-BALANCE-YTD > 250.00
               MOVE 250.00 TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * GR -> JC WHEN LAST MAINT DATE IS PAST
      * JC -> KH WHEN LAST MAINT DATE IS PAST
      * KH -> KN WHEN LAST MAINT DATE IS PAST
      * KN -> PC WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KH' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KH'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KN' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KN'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E647 DESC REQUIRED
      * E523 MIN PAY YTD NEGATIVE
      * E644 MIN PAY YTD OVER 99999
      * E446 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD < 0
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD > 99999.00
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN HELT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = HELM-BALANCE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HELM-BALANCE-YTD - (WS-WORK-AMT * 3)
           COMPUTE HELM-ANNUAL-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE MIN PAY YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3060-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-AMT ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-DISCOUNT / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO HELM-BALANCE-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO HELM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE HELM-BALANCE-AMT ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-BALANCE-AMT > 0 AND HELM-BALANCE-AMT < 5.00
               MOVE 5.00 TO HELM-BALANCE-AMT
           END-IF
           IF HELM-BALANCE-AMT > 35.00
               MOVE 35.00 TO HELM-BALANCE-AMT
           END-IF
           ADD HELM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3080-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-LIMIT-AMT ROUNDED =
                   HELM-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELT-LIMIT-AMT
           END-IF
           ADD HELT-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-APPRAISE.
           IF HELM-APPRAISE-AMT < 100.00
               MOVE 100.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-AMT > 1000.00
               MOVE 1000.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-EFF.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E275 DESC REQUIRED
      * E838 BALANCE YTD NEGATIVE
      * E029 BALANCE YTD OVER 1000000
      * E073 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD < 0
                   MOVE 'E838' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD > 1000000.00
                   MOVE 'E029' TO WS-REJECT-CODE
               WHEN HELM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT SCHED TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-SCHED-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELT-APPRAISE-AMT NOT < WS-LIMIT-SCHED-LOW(WS-TX)
                   MOVE WS-LIMIT-SCHED-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3140-LIMIT-LIMIT.
           IF HELM-LIMIT-YTD < 0.00
               MOVE 0.00 TO HELM-LIMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-LIMIT-YTD > 50000.00
               MOVE 50000.00 TO HELM-LIMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DRAW AMT BY DRAW PERIOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-DRAW-PERIOD-CD
                   ADD HELT-DRAW-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-DRAW-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELM-BALANCE-YTD NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE YTD BY LIEN POS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELM-ANNUAL-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-ANNUAL-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E165 DESC REQUIRED
      * E941 APPRAISE AMT NEGATIVE
      * E061 APPRAISE AMT OVER 10000
      * E997 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT < 0
                   MOVE 'E941' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT > 10000.00
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN HELT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE BAND TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-BAND-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELT-DRAW-AMT NOT < WS-APPRAISE-BAND-LOW(WS-TX)
                   MOVE WS-APPRAISE-BAND-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-AMT * HELM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HELM-ANNUAL-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3240-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-YTD ROUNDED = WS-WORK-AMT-2 *
                       HELM-DISCOUNT / 200
               WHEN HELM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 5.00 TO HELM-BALANCE-YTD
               WHEN HELM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 3.00 TO HELM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE HELM-BALANCE-YTD ROUNDED = WS-WORK-AMT-2 *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-BALANCE-YTD > 0 AND HELM-BALANCE-YTD < 5.00
               MOVE 5.00 TO HELM-BALANCE-YTD
           END-IF
           IF HELM-BALANCE-YTD > 100.00
               MOVE 100.00 TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-EFF.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF HELT-POST-DATE > 0 AND HELT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE YTD FEE ON THE APPRAISE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3270-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-APPRAISE-AMT
                       * HELM-MARGIN / 200
               WHEN HELM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 20.00 TO HELM-APPRAISE-YTD
               WHEN HELM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 15.00 TO HELM-APPRAISE-YTD
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-APPRAISE-AMT
                       * HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-APPRAISE-YTD > 0 AND HELM-APPRAISE-YTD < 1.00
               MOVE 1.00 TO HELM-APPRAISE-YTD
           END-IF
           IF HELM-APPRAISE-YTD > 35.00
               MOVE 35.00 TO HELM-APPRAISE-YTD
           END-IF
           ADD HELM-APPRAISE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3290-LIMIT-LIMIT.
           IF HELM-LIMIT-AMT < 0.00
               MOVE 0.00 TO HELM-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-LIMIT-AMT > 500.00
               MOVE 500.00 TO HELM-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE HELM-DRAW-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-CHANNEL-CD
                   ADD HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-EFF.
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
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRAW AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-DRAW.
           IF HELT-DRAW-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELT-DRAW-AMT * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELM-DRAW-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E027 REJECT CD REQUIRED
      * E525 BALANCE YTD NEGATIVE
      * E238 BALANCE YTD OVER 1000000
      * E059 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-REJECT-CD = SPACES
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD < 0
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD > 1000000.00
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E059' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DRAW AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3360-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-DRAW-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-TYPE-CD = 'CD'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN HELM-TYPE-CD = 'JW'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-DRAW-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LIMIT YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-LIMIT.
           IF HELM-LIMIT-YTD < 10.00
               MOVE 10.00 TO HELM-LIMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-LIMIT-YTD > 1000.00
               MOVE 1000.00 TO HELM-LIMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE BAND TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-BAND-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-BALANCE-AMT NOT < WS-APPRAISE-BAND-LOW(WS-TX)
                   MOVE WS-APPRAISE-BAND-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-OPEN.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE BAND TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-BAND-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-ANNUAL-FEE-AMT NOT < WS-APPRAISE-BAND-LOW(WS-TX)
                   MOVE WS-APPRAISE-BAND-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-APPRAISE.
           IF HELT-APPRAISE-AMT < 0.00
               MOVE 0.00 TO HELT-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-APPRAISE-AMT > 10000.00
               MOVE 10000.00 TO HELT-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-INTEREST.
           IF HELM-INTEREST-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E026 MEMO REQUIRED
      * E358 INTEREST AMT NEGATIVE
      * E602 INTEREST AMT OVER 1000000
      * E065 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3440-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-AMT < 0
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-AMT > 1000000.00
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN HELM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3450-FLAG-APPRAISE.
           IF HELM-APPRAISE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3470-FLAG-APPRAISE.
           IF HELM-APPRAISE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       3480-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-AMT
           END-IF
           ADD HELM-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRAW YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-DRAW.
           IF HELM-DRAW-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-DRAW-YTD * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELM-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY DRAW PERIOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-DRAW-PERIOD-CD
                   ADD HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE APPRAISE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       3520-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-LIMIT-YTD ROUNDED =
                   HELT-APPRAISE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-LIMIT-YTD
           END-IF
           ADD HELM-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3530-FLAG-APPRAISE.
           IF HELM-APPRAISE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E847 CUST ID REQUIRED
      * E606 ANNUAL FEE YTD NEGATIVE
      * E843 ANNUAL FEE YTD OVER 5000
      * E551 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CUST-ID = SPACES
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-YTD < 0
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-YTD > 5000.00
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN HELM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN CLOSE DATE IS PAST
      * PY -> FZ WHEN CLOSE DATE IS PAST
      * FZ -> RC WHEN CLOSE DATE IS PAST
      * RC -> AF WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-LIMIT.
           IF HELM-LIMIT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E087 REF NO REQUIRED
      * E525 DRAW YTD NEGATIVE
      * E542 DRAW YTD OVER 5000
      * E851 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN HELM-DRAW-YTD < 0
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN HELM-DRAW-YTD > 5000.00
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN HELT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E851' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3580-FLAG-BALANCE.
           IF HELM-BALANCE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-BALANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       3590-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-APPRAISE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELT-APPRAISE-AMT
           END-IF
           ADD HELT-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E777 MEMO REQUIRED
      * E578 DRAW AMT NEGATIVE
      * E437 DRAW AMT OVER 25000
      * E460 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT < 0
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT > 25000.00
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN HELM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3610-FLAG-INTEREST.
           IF HELM-INTEREST-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN PAY FEE ON THE BALANCE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3620-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HELM-MIN-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELM-MIN-PAY-AMT ROUNDED = HELM-BALANCE-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 15.00 TO HELM-MIN-PAY-AMT
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 7.00 TO HELM-MIN-PAY-AMT
               WHEN OTHER
                   COMPUTE HELM-MIN-PAY-AMT ROUNDED = HELM-BALANCE-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-MIN-PAY-AMT > 0 AND HELM-MIN-PAY-AMT < 1.00
               MOVE 1.00 TO HELM-MIN-PAY-AMT
           END-IF
           IF HELM-MIN-PAY-AMT > 35.00
               MOVE 35.00 TO HELM-MIN-PAY-AMT
           END-IF
           ADD HELM-MIN-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-LIMIT.
           IF HELT-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELT-LIMIT-AMT * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELM-MIN-PAY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE BAND TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-BAND-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-BALANCE-YTD NOT < WS-APPRAISE-BAND-LOW(WS-TX)
                   MOVE WS-APPRAISE-BAND-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3660-DESCRIBE-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRAW SCHED TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-SCHED-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF HELM-APPRAISE-YTD NOT < WS-DRAW-SCHED-LOW(WS-TX)
                   MOVE WS-DRAW-SCHED-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 12
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE BAND TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3700-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-BAND-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-LIMIT-YTD NOT < WS-APPRAISE-BAND-LOW(WS-TX)
                   MOVE WS-APPRAISE-BAND-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE APPRAISE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ANNUAL FEE YTD.
      *----------------------------------------------------------------*
       3710-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                   HELM-APPRAISE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-YTD
           END-IF
           ADD HELM-ANNUAL-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3720-FLAG-LIMIT.
           IF HELT-LIMIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E168 CONTACT REQUIRED
      * E098 BALANCE YTD NEGATIVE
      * E192 BALANCE YTD OVER 25000
      * E487 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CONTACT = SPACES
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD < 0
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD > 25000.00
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELM-MIN-PAY-YTD NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       3760-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-LIMIT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-LIMIT-YTD
           END-IF
           ADD HELM-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-YTD * HELM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO HELM-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3780-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-LIMIT-AMT ROUNDED =
                   HELM-DRAW-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-LIMIT-AMT
           END-IF
           ADD HELM-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E351 CONTACT REQUIRED
      * E143 MIN PAY YTD NEGATIVE
      * E480 MIN PAY YTD OVER 25000
      * E636 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CONTACT = SPACES
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD < 0
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD > 25000.00
                   MOVE 'E480' TO WS-REJECT-CODE
               WHEN HELM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN LAST MAINT DATE IS PAST
      * JW -> KV WHEN LAST MAINT DATE IS PAST
      * KV -> WS WHEN LAST MAINT DATE IS PAST
      * WS -> XL WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-TYPE.
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
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3820-FLAG-APPRAISE.
           IF HELM-APPRAISE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON APPRAISE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-APPRAISE.
           IF HELT-APPRAISE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELT-APPRAISE-AMT * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELT-ANNUAL-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3890-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 100.00
               MOVE 100.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 500.00
               MOVE 500.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW FEE ON THE LIMIT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3900-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-DRAW-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   COMPUTE HELM-DRAW-AMT ROUNDED = HELM-LIMIT-AMT *
                       HELM-MARGIN / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO HELM-DRAW-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO HELM-DRAW-AMT
               WHEN OTHER
                   COMPUTE HELM-DRAW-AMT ROUNDED = HELM-LIMIT-AMT *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-DRAW-AMT > 0 AND HELM-DRAW-AMT < 1.00
               MOVE 1.00 TO HELM-DRAW-AMT
           END-IF
           IF HELM-DRAW-AMT > 35.00
               MOVE 35.00 TO HELM-DRAW-AMT
           END-IF
           ADD HELM-DRAW-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * APPRAISE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3910-FLAG-APPRAISE.
           IF HELT-APPRAISE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELT-APPRAISE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN LAST MAINT DATE IS PAST
      * NWS -> ZJM WHEN LAST MAINT DATE IS PAST
      * ZJM -> KST WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KST' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-EFF.
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
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3940-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE BAND TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-BAND-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELT-APPRAISE-AMT NOT < WS-APPRAISE-BAND-LOW(WS-TX)
                   MOVE WS-APPRAISE-BAND-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE YTD FEE ON THE BALANCE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3960-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-BALANCE-YTD
                       * HELM-MARGIN / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 5.00 TO HELM-APPRAISE-YTD
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 15.00 TO HELM-APPRAISE-YTD
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-BALANCE-YTD
                       * HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-APPRAISE-YTD > 0 AND HELM-APPRAISE-YTD < 1.00
               MOVE 1.00 TO HELM-APPRAISE-YTD
           END-IF
           IF HELM-APPRAISE-YTD > 50.00
               MOVE 50.00 TO HELM-APPRAISE-YTD
           END-IF
           ADD HELM-APPRAISE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3970-FLAG-LIMIT.
           IF HELM-LIMIT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       3980-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-BALANCE-YTD ROUNDED =
                   HELM-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DRAW AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3990-FLAG-DRAW.
           IF HELM-DRAW-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-DRAW-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4000-FLAG-INTEREST.
           IF HELM-INTEREST-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-INTEREST.
           IF HELM-INTEREST-AMT < 100.00
               MOVE 100.00 TO HELM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-INTEREST-AMT > 1000.00
               MOVE 1000.00 TO HELM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-BALANCE.
           IF HELM-BALANCE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * GR -> JC WHEN LAST PAY DATE IS PAST
      * JC -> KH WHEN LAST PAY DATE IS PAST
      * KH -> KN WHEN LAST PAY DATE IS PAST
      * KN -> PC WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KH' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KH'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KN' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KN'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4050-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELT-DRAW-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELT-DRAW-AMT ROUNDED = HELT-ANNUAL-FEE-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 12.00 TO HELT-DRAW-AMT
               WHEN HELM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 20.00 TO HELT-DRAW-AMT
               WHEN OTHER
                   COMPUTE HELT-DRAW-AMT ROUNDED = HELT-ANNUAL-FEE-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELT-DRAW-AMT > 0 AND HELT-DRAW-AMT < 2.00
               MOVE 2.00 TO HELT-DRAW-AMT
           END-IF
           IF HELT-DRAW-AMT > 100.00
               MOVE 100.00 TO HELT-DRAW-AMT
           END-IF
           ADD HELT-DRAW-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN NEXT DUE DATE IS PAST
      * NWS -> ZJM WHEN NEXT DUE DATE IS PAST
      * ZJM -> KST WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KST' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELM-APPRAISE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELM-APPRAISE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-AMT < 0.00
               MOVE 0.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-AMT > 10000.00
               MOVE 10000.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4110-FLAG-LIMIT.
           IF HELM-LIMIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-TRAN.
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
      * ASSESS THE ANNUAL FEE FEE ON THE DRAW AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4130-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HELT-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED = HELT-DRAW-AMT *
                       HELM-PENALTY-PCT / 200
               WHEN HELM-REGION-CD = 'BSD'
                   MOVE 3.00 TO HELT-ANNUAL-FEE-AMT
               WHEN HELM-REGION-CD = 'HKW'
                   MOVE 5.00 TO HELT-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED = HELT-DRAW-AMT *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELT-ANNUAL-FEE-AMT > 0 AND HELT-ANNUAL-FEE-AMT < 10.00
               MOVE 10.00 TO HELT-ANNUAL-FEE-AMT
           END-IF
           IF HELT-ANNUAL-FEE-AMT > 250.00
               MOVE 250.00 TO HELT-ANNUAL-FEE-AMT
           END-IF
           ADD HELT-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E672 REF NO REQUIRED
      * E567 LIMIT AMT NEGATIVE
      * E345 LIMIT AMT OVER 99999
      * E219 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT < 0
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT > 99999.00
                   MOVE 'E345' TO WS-REJECT-CODE
               WHEN HELM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E768 CONTACT REQUIRED
      * E239 INTEREST AMT NEGATIVE
      * E464 INTEREST AMT OVER 10000
      * E716 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CONTACT = SPACES
                   MOVE 'E768' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-AMT < 0
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-AMT > 10000.00
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-BALANCE.
           IF HELM-BALANCE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE YTD FEE ON THE MIN PAY AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4170-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-MIN-PAY-AMT
                       * HELM-PENALTY-PCT / 200
               WHEN HELM-TYPE-CD = 'CD'
                   MOVE 7.00 TO HELM-APPRAISE-YTD
               WHEN HELM-TYPE-CD = 'JW'
                   MOVE 12.00 TO HELM-APPRAISE-YTD
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-MIN-PAY-AMT
                       * HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-APPRAISE-YTD > 0 AND HELM-APPRAISE-YTD < 10.00
               MOVE 10.00 TO HELM-APPRAISE-YTD
           END-IF
           IF HELM-APPRAISE-YTD > 150.00
               MOVE 150.00 TO HELM-APPRAISE-YTD
           END-IF
           ADD HELM-APPRAISE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE APPRAISE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-BALANCE-YTD ROUNDED =
                   HELT-APPRAISE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE LIMIT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4190-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED = HELM-LIMIT-AMT
                       * HELM-DISCOUNT / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 7.00 TO HELM-ANNUAL-FEE-AMT
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 10.00 TO HELM-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED = HELM-LIMIT-AMT
                       * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-ANNUAL-FEE-AMT > 0 AND HELM-ANNUAL-FEE-AMT < 2.00
               MOVE 2.00 TO HELM-ANNUAL-FEE-AMT
           END-IF
           IF HELM-ANNUAL-FEE-AMT > 50.00
               MOVE 50.00 TO HELM-ANNUAL-FEE-AMT
           END-IF
           ADD HELM-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E222 DESC REQUIRED
      * E666 DRAW AMT NEGATIVE
      * E014 DRAW AMT OVER 5000
      * E223 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT < 0
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT > 5000.00
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN HELT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-STATUS-CD
                   ADD HELM-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-ANNUAL-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REGION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4230-ACCUM-REGION.
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
      * DRAW AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4240-FLAG-DRAW.
           IF HELM-DRAW-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-DRAW-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-CLOSE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE BAND TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-BAND-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-BALANCE-AMT NOT < WS-APPRAISE-BAND-LOW(WS-TX)
                   MOVE WS-APPRAISE-BAND-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4270-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4280-SET-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 12
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4310-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN LAST MAINT DATE IS PAST
      * PVA -> SFC WHEN LAST MAINT DATE IS PAST
      * SFC -> AXX WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4320-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E777 MEMO REQUIRED
      * E910 APPRAISE YTD NEGATIVE
      * E578 APPRAISE YTD OVER 250000
      * E873 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4330-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-YTD < 0
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-YTD > 250000.00
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4340-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 10
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4350-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRAW AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-DRAW.
           IF HELM-DRAW-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-DRAW-AMT * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E406 REF NO REQUIRED
      * E711 MIN PAY AMT NEGATIVE
      * E068 MIN PAY AMT OVER 50000
      * E551 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT < 0
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT > 50000.00
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN HELM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE DRAW YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4380-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-YTD ROUNDED = HELM-DRAW-YTD *
                       HELM-MARGIN / 200
               WHEN HELM-TYPE-CD = 'CD'
                   MOVE 10.00 TO HELM-BALANCE-YTD
               WHEN HELM-TYPE-CD = 'JW'
                   MOVE 12.00 TO HELM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE HELM-BALANCE-YTD ROUNDED = HELM-DRAW-YTD *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-BALANCE-YTD > 0 AND HELM-BALANCE-YTD < 2.00
               MOVE 2.00 TO HELM-BALANCE-YTD
           END-IF
           IF HELM-BALANCE-YTD > 100.00
               MOVE 100.00 TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN PAY YTD.
      *----------------------------------------------------------------*
       4390-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-MIN-PAY-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-MIN-PAY-YTD
           END-IF
           ADD HELM-MIN-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E265 MEMO REQUIRED
      * E669 MIN PAY AMT NEGATIVE
      * E642 MIN PAY AMT OVER 250000
      * E852 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E265' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT < 0
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT > 250000.00
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE BAND TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4410-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-BAND-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELM-BALANCE-AMT NOT < WS-BALANCE-BAND-LOW(WS-TX)
                   MOVE WS-BALANCE-BAND-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4430-FLAG-BALANCE.
           IF HELM-BALANCE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-BALANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DRAW YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4440-FLAG-DRAW.
           IF HELM-DRAW-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-DRAW-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4450-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4460-CHECK-OPEN.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HELM-ANNUAL-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELM-ANNUAL-FEE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE HELM-DRAW-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E423 REF NO REQUIRED
      * E134 ANNUAL FEE AMT NEGATIVE
      * E833 ANNUAL FEE AMT OVER 50000
      * E604 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4480-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E423' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT < 0
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT > 50000.00
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4490-LIMIT-APPRAISE.
           IF HELM-APPRAISE-YTD < 0.00
               MOVE 0.00 TO HELM-APPRAISE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-YTD > 2500.00
               MOVE 2500.00 TO HELM-APPRAISE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4500-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN LAST PAY DATE IS PAST
      * JW -> KV WHEN LAST PAY DATE IS PAST
      * KV -> WS WHEN LAST PAY DATE IS PAST
      * WS -> XL WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4510-SET-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JW' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WS' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE APPRAISE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4520-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELT-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                       HELM-APPRAISE-AMT * HELM-MARGIN / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 15.00 TO HELT-ANNUAL-FEE-AMT
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 15.00 TO HELT-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                       HELM-APPRAISE-AMT * HELM-MARGIN / 100
           END-EVALUATE
           IF HELT-ANNUAL-FEE-AMT > 0 AND HELT-ANNUAL-FEE-AMT < 10.00
               MOVE 10.00 TO HELT-ANNUAL-FEE-AMT
           END-IF
           IF HELT-ANNUAL-FEE-AMT > 50.00
               MOVE 50.00 TO HELT-ANNUAL-FEE-AMT
           END-IF
           ADD HELT-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4530-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW FEE ON THE APPRAISE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4540-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-DRAW-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELM-DRAW-AMT ROUNDED = HELT-APPRAISE-AMT *
                       HELM-PENALTY-PCT / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO HELM-DRAW-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO HELM-DRAW-AMT
               WHEN OTHER
                   COMPUTE HELM-DRAW-AMT ROUNDED = HELT-APPRAISE-AMT *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-DRAW-AMT > 0 AND HELM-DRAW-AMT < 10.00
               MOVE 10.00 TO HELM-DRAW-AMT
           END-IF
           IF HELM-DRAW-AMT > 25.00
               MOVE 25.00 TO HELM-DRAW-AMT
           END-IF
           ADD HELM-DRAW-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4550-FLAG-BALANCE.
           IF HELM-BALANCE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE HELM-DRAW-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E965 CUST ID REQUIRED
      * E039 DRAW AMT NEGATIVE
      * E202 DRAW AMT OVER 99999
      * E943 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4570-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CUST-ID = SPACES
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT < 0
                   MOVE 'E039' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT > 99999.00
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E943' TO WS-REJECT-CODE
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
       4580-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = HELM-BALANCE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELM-BALANCE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * GR -> JC WHEN OPEN DATE IS PAST
      * JC -> KH WHEN OPEN DATE IS PAST
      * KH -> KN WHEN OPEN DATE IS PAST
      * KN -> PC WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4590-SET-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KH' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KH'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KN' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KN'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRAW SCHED TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4600-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-SCHED-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF HELT-ANNUAL-FEE-AMT NOT < WS-DRAW-SCHED-LOW(WS-TX)
                   MOVE WS-DRAW-SCHED-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4610-FLAG-LIMIT.
           IF HELT-LIMIT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE HEL FIELDS TO HEFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-HEFEE.
           INITIALIZE HEFEE-PARMS
           MOVE HELM-INTEREST-AMT
             TO HEFEE-INTEREST-AMT
           MOVE HELM-ANNUAL-FEE-YTD
             TO HEFEE-ANNUAL-FEE-YTD
           MOVE HELM-DRAW-AMT
             TO HEFEE-DRAW-AMT
           MOVE HELM-BALANCE-YTD
             TO HEFEE-BALANCE-YTD
           MOVE HELM-ANNUAL-FEE-AMT
             TO HEFEE-ANNUAL-FEE-AMT
           MOVE HELM-INTEREST-YTD
             TO HEFEE-INTEREST-YTD
           MOVE HELM-OPEN-DATE
             TO HEFEE-OPEN-DATE
           MOVE HELM-NEXT-DUE-DATE
             TO HEFEE-NEXT-DUE-DATE
           MOVE HELM-LAST-PAY-DATE
             TO HEFEE-LAST-PAY-DATE
           MOVE HELM-NEXT-REVIEW-DATE
             TO HEFEE-NEXT-REVIEW-DATE
           MOVE HELM-REASON-CD
             TO HEFEE-REASON-CD
           MOVE HELM-DRAW-PERIOD-CD
             TO HEFEE-DRAW-PERIOD-CD
           MOVE HELM-TYPE-CD
             TO HEFEE-TYPE-CD
           MOVE HELM-STATUS-CD
             TO HEFEE-STATUS-CD
           MOVE HELM-HOLD-FLAG
             TO HEFEE-HOLD-FLAG
           MOVE HELM-PREFERRED-FLAG
             TO HEFEE-PREFERRED-FLAG
           MOVE HELM-NOTICE-SENT-FLAG
             TO HEFEE-NOTICE-SENT-FLAG
           MOVE HELM-NOTICE-CNT
             TO HEFEE-NOTICE-CNT
           MOVE HELM-RETRY-CNT
             TO HEFEE-RETRY-CNT
           MOVE HELM-PENALTY-PCT
             TO HEFEE-PENALTY-PCT
           MOVE HELM-DISCOUNT
             TO HEFEE-DISCOUNT
           MOVE HELM-CONTACT
             TO HEFEE-CONTACT
           MOVE HELM-NAME
             TO HEFEE-NAME
           CALL 'HEFEE' USING HEFEE-PARMS
           EVALUATE HEFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE HEFEE-ANNUAL-FEE-YTD
                     TO HELM-ANNUAL-FEE-YTD
                   MOVE HEFEE-DRAW-AMT
                     TO HELM-DRAW-AMT
                   MOVE HEFEE-BALANCE-YTD
                     TO HELM-BALANCE-YTD
                   MOVE HEFEE-ANNUAL-FEE-AMT
                     TO HELM-ANNUAL-FEE-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-HEFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE HEL FIELDS TO HESTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-HESTAT.
           INITIALIZE HESTAT-PARMS
           MOVE HELM-DRAW-AMT
             TO HESTAT-DRAW-AMT
           MOVE HELM-ANNUAL-FEE-AMT
             TO HESTAT-ANNUAL-FEE-AMT
           MOVE HELM-APPRAISE-YTD
             TO HESTAT-APPRAISE-YTD
           MOVE HELM-BALANCE-AMT
             TO HESTAT-BALANCE-AMT
           MOVE HELM-ANNUAL-FEE-YTD
             TO HESTAT-ANNUAL-FEE-YTD
           MOVE HELM-BALANCE-YTD
             TO HESTAT-BALANCE-YTD
           MOVE HELM-EFFECTIVE-DATE
             TO HESTAT-EFFECTIVE-DATE
           MOVE HELM-OPEN-DATE
             TO HESTAT-OPEN-DATE
           MOVE HELM-LAST-MAINT-DATE
             TO HESTAT-LAST-MAINT-DATE
           MOVE HELM-CLOSE-DATE
             TO HESTAT-CLOSE-DATE
           MOVE HELM-REASON-CD
             TO HESTAT-REASON-CD
           MOVE HELM-TYPE-CD
             TO HESTAT-TYPE-CD
           MOVE HELM-REGION-CD
             TO HESTAT-REGION-CD
           MOVE HELM-DRAW-PERIOD-CD
             TO HESTAT-DRAW-PERIOD-CD
           MOVE HELM-PREFERRED-FLAG
             TO HESTAT-PREFERRED-FLAG
           MOVE HELM-AUTO-RENEW-FLAG
             TO HESTAT-AUTO-RENEW-FLAG
           MOVE HELM-OVERRIDE-FLAG
             TO HESTAT-OVERRIDE-FLAG
           MOVE HELM-RETRY-CNT
             TO HESTAT-RETRY-CNT
           MOVE HELM-PAYMENT-CNT
             TO HESTAT-PAYMENT-CNT
           MOVE HELM-DISCOUNT
             TO HESTAT-DISCOUNT
           MOVE HELM-MARGIN
             TO HESTAT-MARGIN
           MOVE HELM-CONTACT
             TO HESTAT-CONTACT
           MOVE HELM-CUST-ID
             TO HESTAT-CUST-ID
           CALL 'HESTAT' USING HESTAT-PARMS
           EVALUATE HESTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE HESTAT-ANNUAL-FEE-AMT
                     TO HELM-ANNUAL-FEE-AMT
                   MOVE HESTAT-APPRAISE-YTD
                     TO HELM-APPRAISE-YTD
                   MOVE HESTAT-BALANCE-AMT
                     TO HELM-BALANCE-AMT
                   MOVE HESTAT-BALANCE-YTD
                     TO HELM-BALANCE-YTD
                   MOVE HESTAT-PREFERRED-FLAG
                     TO HELM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-HESTAT' TO ABND-PARAGRAPH
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
                   MOVE HELIM-BALANCE-YTD
                     TO HELM-BALANCE-YTD
                   MOVE HELIM-ANNUAL-FEE-AMT
                     TO HELM-ANNUAL-FEE-AMT
                   MOVE HELIM-MIN-PAY-AMT
                     TO HELM-MIN-PAY-AMT
                   MOVE HELIM-AUTO-RENEW-FLAG
                     TO HELM-AUTO-RENEW-FLAG
                   MOVE HELIM-OVERRIDE-FLAG
                     TO HELM-OVERRIDE-FLAG
                   MOVE HELIM-HOLD-FLAG
                     TO HELM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-HELIM' TO ABND-PARAGRAPH
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
           DISPLAY 'HELACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'HELACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
