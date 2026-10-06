      *================================================================*
      * PROGRAM  : TLLARCH                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELLER BALANCING - HISTORY ARCHIVE. READS THE TBL   *
      *            TRANSACTIONS, UPDATES THE TBL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TELLER CASH DRAWER BALANCING,   *
      *            BATCH PROOF AND OVER/SHORT                          *
      * INPUT    : TBLMST (KSDS), TBLTRN, BANKCTL                      *
      * OUTPUT   : TBLOUT, TLLARRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TLDTE, TLPEN, TLWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-07-26 EWS  ORIGINAL                                       *
      * 1991-11-26 DPS  OVERSHORT ROUNDING - HALF UP INC0647           *
      * 1993-04-14 TWH  BATCH NOTICE TEXT PER LEGAL INC4635            *
      * 1994-07-21 LMB  VAULT-BUY ROUNDING - HALF UP PRB7254           *
      * 1995-05-13 TWH  OVERSHORT EXCEPTIONS TO OPS QUEUE CHG8256      *
      * 1996-04-28 LMB  VAULT-BUY TABLE EXPANDED CHG6531               *
      * 2003-01-04 JLM  NEW VAULT-SELL REPORT COLUMNS INC8525          *
      * 2003-09-20 DPS  CASH-OUT ROUNDING - HALF UP INC8468            *
      * 2003-04-14 CRP  PERFORMANCE - OVERSHORT READ AHEAD REQ2446     *
      * 2008-07-24 MKL  CASH-OUT RERUN PROTECTION INC0715              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLLARCH.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TBLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TBLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TBLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TBLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TLLARRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TBLMST.
       FD  TRAN-FILE.
       COPY TBLTRN.
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
                                           VALUE 13.000000.
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
      * WS-CASH-IN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-IN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000075000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000489000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000078500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000450000000090000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001450000000704500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006450000000797500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000136500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000449000'.
       01  WS-CASH-IN-TIER-TBL REDEFINES WS-CASH-IN-TIER-VALUES.
           05  WS-CASH-IN-TIER-ROW OCCURS 8 TIMES.
               10  WS-CASH-IN-TIER-LOW     PIC 9(09)V99.
               10  WS-CASH-IN-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-OUT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000136000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000616000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000085500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000732000'.
       01  WS-CASH-OUT-GRID-TBL REDEFINES WS-CASH-OUT-GRID-VALUES.
           05  WS-CASH-OUT-GRID-ROW OCCURS 4 TIMES.
               10  WS-CASH-OUT-GRID-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VAULT-SE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-SE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000417500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000475500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000583500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000770500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011200000000495500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016200000000895000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026200000000337500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027200000000155500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027300000000660500'.
       01  WS-VAULT-SE-BAND-TBL REDEFINES WS-VAULT-SE-BAND-VALUES.
           05  WS-VAULT-SE-BAND-ROW OCCURS 9 TIMES.
               10  WS-VAULT-SE-BAND-LOW    PIC 9(09)V99.
               10  WS-VAULT-SE-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TLLARCH  TELLER BALANCING'.
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
       COPY TLDTEP.
       COPY TLPENP.
       COPY TLWAIVP.

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
               MOVE 'TBLMST' TO ABND-FILE-NAME
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
           MOVE TBLT-KEY TO TBLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-PROOF-TYPE
               PERFORM 3010-PRORATE-VAULT-BUY
               PERFORM 3020-CHECK-LAST-ACT
               PERFORM 3030-AGE-OPEN
               PERFORM 3040-AGE-NOTICE
               PERFORM 3050-ACCRUE-FEE-PCT
               PERFORM 3060-AGE-POST
               PERFORM 3070-COUNT-SEQ
               PERFORM 3080-DESCRIBE-PROOF-TYPE
               PERFORM 3090-LIMIT-ITEM
               PERFORM 3100-EDIT-REJECT
               PERFORM 3110-SELECT-RATE
               PERFORM 3120-LIMIT-VAULT-BUY
               PERFORM 3130-PRORATE-CASH-IN
               PERFORM 3140-CHECK-NOTICE
               PERFORM 3150-PRORATE-BATCH
               PERFORM 3160-PRORATE-OVERSHORT
               PERFORM 3170-ASSESS-STOP
               PERFORM 3180-SELECT-FEE-PCT
               PERFORM 3190-CHECK-NEXT-REVIEW
               PERFORM 3200-DESCRIBE-CHANNEL
               PERFORM 3210-ASSESS-EXEMPT
               PERFORM 3220-ASSESS-OVERRIDE
               PERFORM 3230-ASSESS-EXEMPT
               PERFORM 3240-AGE-LAST-ACT
               PERFORM 3250-FLAG-OVERSHORT
               PERFORM 3260-ACCRUE-OVERSHORT
               PERFORM 3270-PRORATE-OVERSHORT
               PERFORM 3280-ACCRUE-VAULT-SELL
               PERFORM 3290-EDIT-DESC
               PERFORM 3300-ACCRUE-VAULT-SELL
               PERFORM 3310-ACCUM-TRAN
               PERFORM 3320-DESCRIBE-CHANNEL
               PERFORM 3330-FLAG-VAULT-SELL
               PERFORM 3340-ASSESS-REVERSAL
               PERFORM 3350-DESCRIBE-DRAWER
               PERFORM 3360-DESCRIBE-PROOF-TYPE
               PERFORM 3370-CHECK-LAST-MAINT
               PERFORM 3380-ACCUM-PROOF-TYPE
               PERFORM 3390-FLAG-VAULT-BUY
               PERFORM 3400-ACCRUE-VAULT-SELL
               PERFORM 3410-AGE-NEXT-REVIEW
               PERFORM 3420-FLAG-VAULT-SELL
               PERFORM 3430-FLAG-BATCH
               PERFORM 3440-ASSESS-SUSPEND
               PERFORM 3450-SPLIT-VAULT-SELL
               PERFORM 3460-ACCRUE-CASH-OUT
               PERFORM 3470-FLAG-CASH-IN
               PERFORM 3480-SET-SOURCE
               PERFORM 3490-AGE-OPEN
               PERFORM 3500-LIMIT-VAULT-SELL
               PERFORM 3510-FLAG-OVERSHORT
               PERFORM 3520-LIMIT-OVERSHORT
               PERFORM 3530-LIMIT-BATCH
               PERFORM 3540-EDIT-OPERATOR
               PERFORM 3550-CHECK-OPEN
               PERFORM 3560-CHECK-NEXT-REVIEW
               PERFORM 3570-ACCRUE-OVERSHORT
               PERFORM 3580-DESCRIBE-DRAWER
               PERFORM 3590-COUNT-RETRY
               PERFORM 3600-SPLIT-OVERSHORT
               PERFORM 3610-SET-REASON
               PERFORM 3620-PRORATE-BATCH
               PERFORM 3630-SELECT-RATE
               PERFORM 3640-ACCUM-CHANNEL
               PERFORM 3650-ACCRUE-VAULT-SELL
               PERFORM 3660-COUNT-NOTICE
               PERFORM 3670-PRORATE-OVERSHORT
               PERFORM 3680-ACCUM-CHANNEL
               PERFORM 3690-ACCRUE-VAULT-SELL
               PERFORM 3700-LIMIT-OVERSHORT
               PERFORM 3710-SPLIT-CASH-IN
               PERFORM 3720-LIMIT-VAULT-SELL
               PERFORM 3730-LIMIT-OVERSHORT
               PERFORM 3740-COUNT-SEQ
               PERFORM 3750-ACCRUE-BATCH
               PERFORM 3760-ACCUM-PROOF-TYPE
               PERFORM 3770-SPLIT-VAULT-SELL
               PERFORM 3780-ASSESS-EXEMPT
               PERFORM 3790-PRORATE-CASH-OUT
               PERFORM 3800-SELECT-DISCOUNT
               PERFORM 3810-LIMIT-BATCH
               PERFORM 3820-EDIT-NAME
               PERFORM 3830-ACCRUE-CASH-IN
               PERFORM 3840-DESCRIBE-DRAWER
               PERFORM 3850-SPLIT-VAULT-BUY
               PERFORM 3860-ACCUM-REASON
               PERFORM 3870-ASSESS-OVERRIDE
               PERFORM 3880-SELECT-RATE
               PERFORM 3890-CHECK-LAST-ACT
               PERFORM 3900-ACCRUE-CASH-IN
               PERFORM 3910-ASSESS-EXEMPT
               PERFORM 3920-ASSESS-REVIEW
               PERFORM 3930-ASSESS-EXEMPT
               PERFORM 3940-ACCRUE-VAULT-SELL
               PERFORM 3950-EDIT-REJECT
               PERFORM 3960-EDIT-CONTACT
               PERFORM 3970-EDIT-MEMO
               PERFORM 3980-LIMIT-VAULT-SELL
               PERFORM 3990-ACCUM-CHANNEL
               PERFORM 4000-LIMIT-VAULT-SELL
               PERFORM 4010-FLAG-VAULT-SELL
               PERFORM 4020-ASSESS-OVERRIDE
               PERFORM 4030-SPLIT-BATCH
               PERFORM 4040-SPLIT-VAULT-BUY
               PERFORM 4050-PRORATE-VAULT-BUY
               PERFORM 4060-SELECT-DISCOUNT
               PERFORM 4070-SELECT-DISCOUNT
               PERFORM 4080-PRORATE-OVERSHORT
               PERFORM 4090-EDIT-NAME
               PERFORM 4100-FLAG-VAULT-SELL
               PERFORM 4110-FLAG-CASH-OUT
               PERFORM 4120-DESCRIBE-CYCLE
               PERFORM 4130-COUNT-EXCEPT
               PERFORM 4140-ACCRUE-BATCH
               PERFORM 4150-ACCRUE-CASH-OUT
               PERFORM 4160-ASSESS-REVERSAL
               PERFORM 4170-SPLIT-VAULT-BUY
               PERFORM 4180-ASSESS-HOLD
               PERFORM 4190-COUNT-SEQ
               PERFORM 4200-SELECT-DISCOUNT
               PERFORM 4210-AGE-OPEN
               PERFORM 4220-FLAG-VAULT-BUY
               PERFORM 4230-ASSESS-OVERRIDE
               PERFORM 4240-SET-CYCLE
               PERFORM 4250-PRORATE-VAULT-BUY
               PERFORM 4260-SELECT-RATE
               PERFORM 4270-SPLIT-VAULT-SELL
               PERFORM 4280-COUNT-EXCEPT
               PERFORM 4290-DESCRIBE-DRAWER
               PERFORM 4300-EDIT-REJECT
               PERFORM 4310-ASSESS-STOP
               PERFORM 4320-SELECT-DISCOUNT
               PERFORM 4330-COUNT-RETRY
               PERFORM 4340-COUNT-RETRY
               PERFORM 4350-PRORATE-CASH-IN
               PERFORM 4360-PRORATE-BATCH
               PERFORM 4370-COUNT-EXCEPT
               PERFORM 4380-ASSESS-EXEMPT
               PERFORM 4390-PRORATE-BATCH
               PERFORM 4400-DESCRIBE-CHANNEL
               PERFORM 4410-PRORATE-BATCH
               PERFORM 4420-FLAG-VAULT-SELL
               PERFORM 4430-AGE-LAST-ACT
               PERFORM 4440-CHECK-LAST-ACT
               PERFORM 4450-ACCRUE-CASH-IN
               PERFORM 4460-SELECT-DISCOUNT
               PERFORM 4470-PRORATE-CASH-OUT
               PERFORM 4480-DESCRIBE-TRAN
               PERFORM 4490-FLAG-OVERSHORT
               PERFORM 4500-SET-CHANNEL
               PERFORM 4510-FLAG-CASH-OUT
               PERFORM 4520-ACCUM-PROOF-TYPE
               PERFORM 4530-DESCRIBE-TRAN
               PERFORM 4540-SPLIT-OVERSHORT
               PERFORM 4550-LIMIT-OVERSHORT
               PERFORM 4560-EDIT-REF-NO
               PERFORM 4570-SET-CYCLE
               PERFORM 4580-ASSESS-REVERSAL
               PERFORM 4590-DESCRIBE-CYCLE
               PERFORM 4600-AGE-NOTICE
               PERFORM 4610-SPLIT-VAULT-SELL
               PERFORM 4620-EDIT-ADDR
               PERFORM 7010-CALL-TLDTE
               PERFORM 7040-CALL-TLPEN
               PERFORM 7020-CALL-TLWAIV
               REWRITE TBL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TBLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TBL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE PROOF TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3010-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-BUY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 6
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PROOF TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E093 REJECT CD REQUIRED
      * E141 CASH IN YTD NEGATIVE
      * E188 CASH IN YTD OVER 250000
      * E820 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD < 0
                   MOVE 'E141' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD > 250000.00
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN TBLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE BAND TIER FOR THE VAULT BUY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-BAND-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLM-VAULT-BUY-YTD NOT < WS-VAULT-SE-BAND-LOW(WS-TX)
                   MOVE WS-VAULT-SE-BAND-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3120-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD < 25.00
               MOVE 25.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-YTD > 5000.00
               MOVE 5000.00 TO TBLM-VAULT-BUY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                   TBLM-VAULT-BUY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-BATCH-AMT ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       3160-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3170-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                       TBLM-VAULT-SELL-AMT * TBLM-RATE / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 7.00 TO TBLM-CASH-IN-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 7.00 TO TBLM-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                       TBLM-VAULT-SELL-AMT * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-CASH-IN-AMT > 0 AND TBLM-CASH-IN-AMT < 5.00
               MOVE 5.00 TO TBLM-CASH-IN-AMT
           END-IF
           IF TBLM-CASH-IN-AMT > 75.00
               MOVE 75.00 TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE OVERSHORT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-OVERSHORT-YTD NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TBLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3210-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLT-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                       TBLT-OVERSHORT-AMT * TBLM-RATE / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 7.00 TO TBLT-CASH-OUT-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 7.00 TO TBLT-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                       TBLT-OVERSHORT-AMT * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-CASH-OUT-AMT > 0 AND TBLT-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TBLT-CASH-OUT-AMT
           END-IF
           IF TBLT-CASH-OUT-AMT > 25.00
               MOVE 25.00 TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE CASH OUT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3220-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-CASH-OUT-YTD
                       * TBLM-DISCOUNT / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 10.00 TO TBLM-CASH-IN-YTD
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 10.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-CASH-OUT-YTD
                       * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 10.00
               MOVE 10.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 150.00
               MOVE 150.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE BATCH YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TBLM-BATCH-YTD *
                       TBLM-FEE-PCT / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TBLM-BATCH-YTD *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3250-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLT-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E016 DESC REQUIRED
      * E824 CASH OUT YTD NEGATIVE
      * E100 CASH OUT YTD OVER 250000
      * E147 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3290-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD < 0
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD > 250000.00
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-OUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-TRAN-CD
                   ADD TBLM-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-VAULT-BUY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3330-FLAG-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-SELL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE VAULT SELL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3340-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLT-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                       TBLM-VAULT-SELL-YTD * TBLM-FEE-PCT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 12.00 TO TBLT-VAULT-SELL-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 10.00 TO TBLT-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                       TBLM-VAULT-SELL-YTD * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLT-VAULT-SELL-AMT > 0 AND TBLT-VAULT-SELL-AMT < 2.00
               MOVE 2.00 TO TBLT-VAULT-SELL-AMT
           END-IF
           IF TBLT-VAULT-SELL-AMT > 25.00
               MOVE 25.00 TO TBLT-VAULT-SELL-AMT
           END-IF
           ADD TBLT-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PROOF TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD TBLM-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3390-FLAG-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-BUY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3400-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0 AND TBLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-BATCH.
           IF TBLM-BATCH-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT YTD FEE ON THE BATCH YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3440-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-YTD ROUNDED = TBLM-BATCH-YTD *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 20.00 TO TBLM-OVERSHORT-YTD
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 5.00 TO TBLM-OVERSHORT-YTD
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-YTD ROUNDED = TBLM-BATCH-YTD *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-YTD > 0 AND TBLM-OVERSHORT-YTD < 5.00
               MOVE 5.00 TO TBLM-OVERSHORT-YTD
           END-IF
           IF TBLM-OVERSHORT-YTD > 50.00
               MOVE 50.00 TO TBLM-OVERSHORT-YTD
           END-IF
           ADD TBLM-OVERSHORT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-OUT-YTD * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3470-FLAG-CASH-IN.
           IF TBLM-CASH-IN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN NEXT REVIEW DATE IS PAST
      * PT -> SZ WHEN NEXT REVIEW DATE IS PAST
      * SZ -> VK WHEN NEXT REVIEW DATE IS PAST
      * VK -> VW WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PT' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VK' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VW' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
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
      * VAULT SELL YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD < 50.00
               MOVE 50.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-YTD > 5000.00
               MOVE 5000.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-YTD < 10.00
               MOVE 10.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-YTD > 50000.00
               MOVE 50000.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-BATCH.
           IF TBLM-BATCH-AMT < 0.00
               MOVE 0.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-AMT > 50000.00
               MOVE 50000.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E673 OPERATOR REQUIRED
      * E724 OVERSHORT YTD NEGATIVE
      * E272 OVERSHORT YTD OVER 99999
      * E349 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD < 0
                   MOVE 'E724' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD > 99999.00
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-YTD * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLT-OVERSHORT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLT-OVERSHORT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TBLM-CASH-IN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN LAST PAY DATE IS PAST
      * GV -> PG WHEN LAST PAY DATE IS PAST
      * PG -> SX WHEN LAST PAY DATE IS PAST
      * SX -> XN WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SX' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SX'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XN' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-BATCH-AMT ROUNDED =
                   TBLM-VAULT-BUY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE BAND TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3630-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-BAND-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLT-VAULT-SELL-AMT NOT < WS-VAULT-SE-BAND-LOW(WS-TX)
                   MOVE WS-VAULT-SE-BAND-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-VAULT-SELL-AMT * TBLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TBLM-BATCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD TBLM-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TBLT-CASH-OUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-OVERSHORT.
           IF TBLT-OVERSHORT-AMT < 10.00
               MOVE 10.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-OVERSHORT-AMT > 1000.00
               MOVE 1000.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH IN YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TBLM-CASH-IN-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-IN-YTD - (WS-WORK-AMT * 2)
           COMPUTE TBLT-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3720-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT < 50.00
               MOVE 50.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 500.00
               MOVE 500.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-OVERSHORT.
           IF TBLT-OVERSHORT-AMT < 25.00
               MOVE 25.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-OVERSHORT-AMT > 5000.00
               MOVE 5000.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 10
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-BATCH.
           IF TBLT-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-BATCH-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT BUY YTD BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD TBLM-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-VAULT-BUY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TBLM-OVERSHORT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE OVERSHORT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3780-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-OVERSHORT-YTD
                       * TBLM-FEE-PCT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 7.00 TO TBLM-CASH-IN-AMT
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 12.00 TO TBLM-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-OVERSHORT-YTD
                       * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-AMT > 0 AND TBLM-CASH-IN-AMT < 2.00
               MOVE 2.00 TO TBLM-CASH-IN-AMT
           END-IF
           IF TBLM-CASH-IN-AMT > 250.00
               MOVE 250.00 TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       3790-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-OUT-YTD ROUNDED =
                   TBLM-VAULT-SELL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-OUT-YTD
           END-IF
           ADD TBLM-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-CASH-OUT-YTD NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-BATCH.
           IF TBLM-BATCH-YTD < 10.00
               MOVE 10.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-YTD > 50000.00
               MOVE 50000.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E464 NAME REQUIRED
      * E858 OVERSHORT AMT NEGATIVE
      * E379 OVERSHORT AMT OVER 250000
      * E460 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-NAME = SPACES
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT < 0
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT > 250000.00
                   MOVE 'E379' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-CASH-IN.
           IF TBLM-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLT-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-VAULT-BUY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLT-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE CASH OUT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3870-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-RATE / 200
               WHEN TBLM-CYCLE-CD = 'KX'
                   MOVE 7.00 TO TBLM-OVERSHORT-AMT
               WHEN TBLM-CYCLE-CD = 'TA'
                   MOVE 3.00 TO TBLM-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-AMT > 0 AND TBLM-OVERSHORT-AMT < 2.00
               MOVE 2.00 TO TBLM-OVERSHORT-AMT
           END-IF
           IF TBLM-OVERSHORT-AMT > 50.00
               MOVE 50.00 TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE BAND TIER FOR THE CASH OUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-BAND-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLM-CASH-OUT-YTD NOT < WS-VAULT-SE-BAND-LOW(WS-TX)
                   MOVE WS-VAULT-SE-BAND-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-CASH-IN.
           IF TBLM-CASH-IN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-IN-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-VAULT-SELL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3910-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-PROOF-TYPE-CD = 'JG'
                   MOVE 5.00 TO TBLM-CASH-IN-YTD
               WHEN TBLM-PROOF-TYPE-CD = 'RX'
                   MOVE 15.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 2.00
               MOVE 2.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 75.00
               MOVE 75.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE CASH OUT AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3920-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TBLT-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLT-CASH-OUT-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 10.00 TO TBLT-BATCH-AMT
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 5.00 TO TBLT-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLT-CASH-OUT-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLT-BATCH-AMT > 0 AND TBLT-BATCH-AMT < 1.00
               MOVE 1.00 TO TBLT-BATCH-AMT
           END-IF
           IF TBLT-BATCH-AMT > 100.00
               MOVE 100.00 TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE CASH OUT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3930-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-CASH-OUT-YTD
                       * TBLM-RATE / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO TBLM-CASH-IN-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO TBLM-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-CASH-OUT-YTD
                       * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-CASH-IN-AMT > 0 AND TBLM-CASH-IN-AMT < 2.00
               MOVE 2.00 TO TBLM-CASH-IN-AMT
           END-IF
           IF TBLM-CASH-IN-AMT > 150.00
               MOVE 150.00 TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLT-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E268 REJECT CD REQUIRED
      * E651 CASH OUT YTD NEGATIVE
      * E379 CASH OUT YTD OVER 250000
      * E009 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD < 0
                   MOVE 'E651' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD > 250000.00
                   MOVE 'E379' TO WS-REJECT-CODE
               WHEN TBLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E862 CONTACT REQUIRED
      * E744 VAULT BUY YTD NEGATIVE
      * E292 VAULT BUY YTD OVER 50000
      * E256 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E862' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-YTD < 0
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-YTD > 50000.00
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E256' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E325 MEMO REQUIRED
      * E320 VAULT SELL AMT NEGATIVE
      * E664 VAULT SELL AMT OVER 10000
      * E776 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-MEMO = SPACES
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-AMT < 0
                   MOVE 'E320' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-AMT > 10000.00
                   MOVE 'E664' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT < 25.00
               MOVE 25.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-VAULT-SELL-AMT > 500.00
               MOVE 500.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4000-LIMIT-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT < 100.00
               MOVE 100.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-VAULT-SELL-AMT > 500.00
               MOVE 500.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4010-FLAG-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-SELL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4020-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO TBLM-CASH-IN-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO TBLM-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-AMT > 0 AND TBLM-CASH-IN-AMT < 5.00
               MOVE 5.00 TO TBLM-CASH-IN-AMT
           END-IF
           IF TBLM-CASH-IN-AMT > 35.00
               MOVE 35.00 TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE TBLM-VAULT-BUY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-BUY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-BUY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BATCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT BUY YTD.
      *----------------------------------------------------------------*
       4050-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-BUY-YTD ROUNDED =
                   TBLT-BATCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-VAULT-BUY-YTD
           END-IF
           ADD TBLM-VAULT-BUY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE OVERSHORT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-OVERSHORT-YTD NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE BATCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-BATCH-AMT NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E299 NAME REQUIRED
      * E139 OVERSHORT YTD NEGATIVE
      * E369 OVERSHORT YTD OVER 5000
      * E571 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-NAME = SPACES
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD < 0
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-YTD > 5000.00
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4100-FLAG-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4110-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-BATCH.
           IF TBLM-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-AMT * TBLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-CASH-OUT-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-OUT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE CASH IN YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4160-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLT-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED = TBLM-CASH-IN-YTD
                       * TBLM-RATE / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 15.00 TO TBLT-CASH-OUT-AMT
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 3.00 TO TBLT-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED = TBLM-CASH-IN-YTD
                       * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-CASH-OUT-AMT > 0 AND TBLT-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TBLT-CASH-OUT-AMT
           END-IF
           IF TBLT-CASH-OUT-AMT > 150.00
               MOVE 150.00 TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-BUY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-BUY-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TBLT-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL YTD FEE ON THE BATCH AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4180-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-SELL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-SELL-YTD ROUNDED = TBLT-BATCH-AMT
                       * TBLM-RATE / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 7.00 TO TBLM-VAULT-SELL-YTD
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 10.00 TO TBLM-VAULT-SELL-YTD
               WHEN OTHER
                   COMPUTE TBLM-VAULT-SELL-YTD ROUNDED = TBLT-BATCH-AMT
                       * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-VAULT-SELL-YTD > 0 AND TBLM-VAULT-SELL-YTD < 1.00
               MOVE 1.00 TO TBLM-VAULT-SELL-YTD
           END-IF
           IF TBLM-VAULT-SELL-YTD > 35.00
               MOVE 35.00 TO TBLM-VAULT-SELL-YTD
           END-IF
           ADD TBLM-VAULT-SELL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4190-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 12
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TBLM-CASH-IN-AMT NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4210-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-BUY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY FEE ON THE CASH OUT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4230-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-BUY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-BUY-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-FEE-PCT / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO TBLM-VAULT-BUY-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO TBLM-VAULT-BUY-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-BUY-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-VAULT-BUY-AMT > 0 AND TBLM-VAULT-BUY-AMT < 2.00
               MOVE 2.00 TO TBLM-VAULT-BUY-AMT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 25.00
               MOVE 25.00 TO TBLM-VAULT-BUY-AMT
           END-IF
           ADD TBLM-VAULT-BUY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN EFFECTIVE DATE IS PAST
      * TA -> TG WHEN EFFECTIVE DATE IS PAST
      * TG -> WG WHEN EFFECTIVE DATE IS PAST
      * WG -> KX WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4240-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT BUY AMT.
      *----------------------------------------------------------------*
       4250-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-BUY-AMT ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-BUY-AMT
           END-IF
           ADD TBLM-VAULT-BUY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4270-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TBLM-CASH-IN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4280-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E987 REJECT CD REQUIRED
      * E926 BATCH YTD NEGATIVE
      * E180 BATCH YTD OVER 1000000
      * E993 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD < 0
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD > 1000000.00
                   MOVE 'E180' TO WS-REJECT-CODE
               WHEN TBLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE CASH OUT YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4310-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-AMT ROUNDED = TBLM-CASH-OUT-YTD *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 15.00 TO TBLM-BATCH-AMT
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 5.00 TO TBLM-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLM-BATCH-AMT ROUNDED = TBLM-CASH-OUT-YTD *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-BATCH-AMT > 0 AND TBLM-BATCH-AMT < 5.00
               MOVE 5.00 TO TBLM-BATCH-AMT
           END-IF
           IF TBLM-BATCH-AMT > 25.00
               MOVE 25.00 TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT GRID TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4320-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLT-VAULT-SELL-AMT NOT < WS-CASH-OUT-GRID-LOW(WS-TX)
                   MOVE WS-CASH-OUT-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4330-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4340-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       4350-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       4360-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-BATCH-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH YTD FEE ON THE CASH OUT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4380-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLT-CASH-OUT-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 20.00 TO TBLM-BATCH-YTD
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 15.00 TO TBLM-BATCH-YTD
               WHEN OTHER
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLT-CASH-OUT-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-BATCH-YTD > 0 AND TBLM-BATCH-YTD < 5.00
               MOVE 5.00 TO TBLM-BATCH-YTD
           END-IF
           IF TBLM-BATCH-YTD > 50.00
               MOVE 50.00 TO TBLM-BATCH-YTD
           END-IF
           ADD TBLM-BATCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE BATCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4390-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TBLM-BATCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4400-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       4410-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-BATCH-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VAULT SELL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4420-FLAG-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-SELL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4430-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4440-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4450-ACCRUE-CASH-IN.
           IF TBLM-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLT-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN TIER TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4460-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-TIER-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TBLM-OVERSHORT-AMT NOT < WS-CASH-IN-TIER-LOW(WS-TX)
                   MOVE WS-CASH-IN-TIER-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       4470-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   TBLM-OVERSHORT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4480-DESCRIBE-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BO'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'OS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4490-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4500-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4510-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT YTD BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4520-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD TBLM-CASH-OUT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-OUT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4530-DESCRIBE-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BO'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'OS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4540-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE TBLM-CASH-IN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4550-LIMIT-OVERSHORT.
           IF TBLT-OVERSHORT-AMT < 100.00
               MOVE 100.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-OVERSHORT-AMT > 1000.00
               MOVE 1000.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E217 REF NO REQUIRED
      * E010 VAULT SELL AMT NEGATIVE
      * E448 VAULT SELL AMT OVER 25000
      * E815 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4560-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-REF-NO = SPACES
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN TBLT-VAULT-SELL-AMT < 0
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN TBLT-VAULT-SELL-AMT > 25000.00
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN EFFECTIVE DATE IS PAST
      * TA -> TG WHEN EFFECTIVE DATE IS PAST
      * TG -> WG WHEN EFFECTIVE DATE IS PAST
      * WG -> KX WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4570-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH YTD FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4580-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLT-OVERSHORT-AMT *
                       TBLM-RATE / 200
               WHEN TBLM-CYCLE-CD = 'KX'
                   MOVE 15.00 TO TBLM-BATCH-YTD
               WHEN TBLM-CYCLE-CD = 'TA'
                   MOVE 5.00 TO TBLM-BATCH-YTD
               WHEN OTHER
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLT-OVERSHORT-AMT *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-BATCH-YTD > 0 AND TBLM-BATCH-YTD < 5.00
               MOVE 5.00 TO TBLM-BATCH-YTD
           END-IF
           IF TBLM-BATCH-YTD > 100.00
               MOVE 100.00 TO TBLM-BATCH-YTD
           END-IF
           ADD TBLM-BATCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4590-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4600-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4610-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-YTD - (WS-WORK-AMT *
               3)
           COMPUTE TBLM-VAULT-BUY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E063 ADDR REQUIRED
      * E230 WORK AMT 2 NEGATIVE
      * E888 WORK AMT 2 OVER 250000
      * E393 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4620-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN TBLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TLDTE.
           INITIALIZE TLDTE-PARMS
           MOVE TBLM-CASH-IN-YTD
             TO TLDTE-CASH-IN-YTD
           MOVE TBLM-CASH-OUT-YTD
             TO TLDTE-CASH-OUT-YTD
           MOVE TBLM-CASH-OUT-AMT
             TO TLDTE-CASH-OUT-AMT
           MOVE TBLM-VAULT-BUY-YTD
             TO TLDTE-VAULT-BUY-YTD
           MOVE TBLM-BATCH-AMT
             TO TLDTE-BATCH-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLDTE-CASH-IN-AMT
           MOVE TBLM-LAST-STMT-DATE
             TO TLDTE-LAST-STMT-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLDTE-LAST-ACT-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLDTE-NOTICE-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLDTE-EFFECTIVE-DATE
           MOVE TBLM-CYCLE-CD
             TO TLDTE-CYCLE-CD
           MOVE TBLM-CHANNEL-CD
             TO TLDTE-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLDTE-SOURCE-CD
           MOVE TBLM-DRAWER-CD
             TO TLDTE-DRAWER-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLDTE-SUSPEND-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLDTE-HOLD-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLDTE-STOP-FLAG
           MOVE TBLM-NOTICE-CNT
             TO TLDTE-NOTICE-CNT
           MOVE TBLM-RETRY-CNT
             TO TLDTE-RETRY-CNT
           MOVE TBLM-DISCOUNT
             TO TLDTE-DISCOUNT
           MOVE TBLM-FEE-PCT
             TO TLDTE-FEE-PCT
           MOVE TBLM-DESC
             TO TLDTE-DESC
           MOVE TBLM-CONTACT
             TO TLDTE-CONTACT
           CALL 'TLDTE' USING TLDTE-PARMS
           EVALUATE TLDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLDTE-CASH-IN-YTD
                     TO TBLM-CASH-IN-YTD
                   MOVE TLDTE-CASH-OUT-YTD
                     TO TBLM-CASH-OUT-YTD
                   MOVE TLDTE-CASH-OUT-AMT
                     TO TBLM-CASH-OUT-AMT
                   MOVE TLDTE-VAULT-BUY-YTD
                     TO TBLM-VAULT-BUY-YTD
                   MOVE TLDTE-BATCH-AMT
                     TO TBLM-BATCH-AMT
                   MOVE TLDTE-STOP-FLAG
                     TO TBLM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TLDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-TLPEN.
           INITIALIZE TLPEN-PARMS
           MOVE TBLM-OVERSHORT-AMT
             TO TLPEN-OVERSHORT-AMT
           MOVE TBLM-VAULT-BUY-AMT
             TO TLPEN-VAULT-BUY-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLPEN-CASH-IN-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLPEN-CASH-OUT-YTD
           MOVE TBLM-BATCH-AMT
             TO TLPEN-BATCH-AMT
           MOVE TBLM-CASH-OUT-AMT
             TO TLPEN-CASH-OUT-AMT
           MOVE TBLM-OPEN-DATE
             TO TLPEN-OPEN-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLPEN-EFFECTIVE-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLPEN-NOTICE-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLPEN-NEXT-REVIEW-DATE
           MOVE TBLM-DRAWER-CD
             TO TLPEN-DRAWER-CD
           MOVE TBLM-REASON-CD
             TO TLPEN-REASON-CD
           MOVE TBLM-CHANNEL-CD
             TO TLPEN-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLPEN-SOURCE-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLPEN-SUSPEND-FLAG
           MOVE TBLM-REVIEW-FLAG
             TO TLPEN-REVIEW-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLPEN-HOLD-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLPEN-ITEM-CNT
           MOVE TBLM-RETRY-CNT
             TO TLPEN-RETRY-CNT
           MOVE TBLM-RATE
             TO TLPEN-RATE
           MOVE TBLM-FEE-PCT
             TO TLPEN-FEE-PCT
           MOVE TBLM-REF-NO
             TO TLPEN-REF-NO
           MOVE TBLM-DESC
             TO TLPEN-DESC
           CALL 'TLPEN' USING TLPEN-PARMS
           EVALUATE TLPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLPEN-VAULT-BUY-AMT
                     TO TBLM-VAULT-BUY-AMT
                   MOVE TLPEN-CASH-IN-AMT
                     TO TBLM-CASH-IN-AMT
                   MOVE TLPEN-CASH-OUT-AMT
                     TO TBLM-CASH-OUT-AMT
                   MOVE TLPEN-HOLD-FLAG
                     TO TBLM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-TLPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TLWAIV.
           INITIALIZE TLWAIV-PARMS
           MOVE TBLM-VAULT-SELL-AMT
             TO TLWAIV-VAULT-SELL-AMT
           MOVE TBLM-VAULT-BUY-YTD
             TO TLWAIV-VAULT-BUY-YTD
           MOVE TBLM-BATCH-YTD
             TO TLWAIV-BATCH-YTD
           MOVE TBLM-CASH-IN-AMT
             TO TLWAIV-CASH-IN-AMT
           MOVE TBLM-CASH-IN-YTD
             TO TLWAIV-CASH-IN-YTD
           MOVE TBLM-CASH-OUT-YTD
             TO TLWAIV-CASH-OUT-YTD
           MOVE TBLM-EFFECTIVE-DATE
             TO TLWAIV-EFFECTIVE-DATE
           MOVE TBLM-LAST-STMT-DATE
             TO TLWAIV-LAST-STMT-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLWAIV-LAST-PAY-DATE
           MOVE TBLM-OPEN-DATE
             TO TLWAIV-OPEN-DATE
           MOVE TBLM-SOURCE-CD
             TO TLWAIV-SOURCE-CD
           MOVE TBLM-DRAWER-CD
             TO TLWAIV-DRAWER-CD
           MOVE TBLM-CHANNEL-CD
             TO TLWAIV-CHANNEL-CD
           MOVE TBLM-REASON-CD
             TO TLWAIV-REASON-CD
           MOVE TBLM-STOP-FLAG
             TO TLWAIV-STOP-FLAG
           MOVE TBLM-SUSPEND-FLAG
             TO TLWAIV-SUSPEND-FLAG
           MOVE TBLM-EXEMPT-FLAG
             TO TLWAIV-EXEMPT-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLWAIV-ITEM-CNT
           MOVE TBLM-EXCEPT-CNT
             TO TLWAIV-EXCEPT-CNT
           MOVE TBLM-FEE-PCT
             TO TLWAIV-FEE-PCT
           MOVE TBLM-RATE
             TO TLWAIV-RATE
           MOVE TBLM-DESC
             TO TLWAIV-DESC
           MOVE TBLM-REF-NO
             TO TLWAIV-REF-NO
           CALL 'TLWAIV' USING TLWAIV-PARMS
           EVALUATE TLWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLWAIV-VAULT-BUY-YTD
                     TO TBLM-VAULT-BUY-YTD
                   MOVE TLWAIV-BATCH-YTD
                     TO TBLM-BATCH-YTD
                   MOVE TLWAIV-CASH-IN-AMT
                     TO TBLM-CASH-IN-AMT
                   MOVE TLWAIV-STOP-FLAG
                     TO TBLM-STOP-FLAG
                   MOVE TLWAIV-SUSPEND-FLAG
                     TO TBLM-SUSPEND-FLAG
                   MOVE TLWAIV-EXEMPT-FLAG
                     TO TBLM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TLWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TBLT-KEY TO RD-KEY
           MOVE TBLT-TRAN-CD TO RD-CODE
           MOVE TBLT-CASH-OUT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TBLT-KEY TO RD-KEY
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
           DISPLAY 'TLLARCH READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TLLARCH' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
