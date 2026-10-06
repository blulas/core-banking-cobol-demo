      *================================================================*
      * PROGRAM  : HELSWEP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : HOME EQUITY LINES - SWEEP. READS THE HEL            *
      *            TRANSACTIONS, UPDATES THE HEL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOME EQUITY LINE DRAWS, BILLING,*
      *            RATE CHANGE AND FREEZE                              *
      * INPUT    : HELMST (KSDS), HELTRN, BANKCTL                      *
      * OUTPUT   : HELOUT, HELSWRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, HEFEE, HELIM            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-10 TWH  ORIGINAL                                       *
      * 1994-04-02 SKN  ANNUAL-FEE FEE SCHEDULE UPDATE INC2007         *
      * 1995-03-02 EWS  MIN-PAY RERUN PROTECTION REQ0722               *
      * 1999-07-23 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-01-16 PQR  BALANCE TABLE EXPANDED CHG1620                 *
      * 2005-08-05 MKL  MIN-PAY LIMIT REVISED REQ8297                  *
      * 2008-07-20 TWH  NEW MIN-PAY REPORT COLUMNS REQ0965             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELSWEP.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "HELSWRPT"
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
                                           VALUE 22.000000.
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
      * WS-MIN-PAY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000404000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000158500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000053000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000260000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000628000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010650000000714000'.
       01  WS-MIN-PAY-TIER-TBL REDEFINES WS-MIN-PAY-TIER-VALUES.
           05  WS-MIN-PAY-TIER-ROW OCCURS 7 TIMES.
               10  WS-MIN-PAY-TIER-LOW     PIC 9(09)V99.
               10  WS-MIN-PAY-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DRAW-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRAW-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000095000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000459500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000169500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000366500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000092500'.
       01  WS-DRAW-GRID-TBL REDEFINES WS-DRAW-GRID-VALUES.
           05  WS-DRAW-GRID-ROW OCCURS 6 TIMES.
               10  WS-DRAW-GRID-LOW        PIC 9(09)V99.
               10  WS-DRAW-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-APPRAISE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-APPRAISE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000710000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000185000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000027000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000681500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000832000'.
       01  WS-APPRAISE-TIER-TBL REDEFINES WS-APPRAISE-TIER-VALUES.
           05  WS-APPRAISE-TIER-ROW OCCURS 6 TIMES.
               10  WS-APPRAISE-TIER-LOW    PIC 9(09)V99.
               10  WS-APPRAISE-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  HELSWEP  HOME EQUITY LINES'.
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
               PERFORM 3000-DESCRIBE-DRAW-PERIOD
               PERFORM 3010-COUNT-PAYMENT
               PERFORM 3020-ACCRUE-ANNUAL-FEE
               PERFORM 3030-LIMIT-ITEM
               PERFORM 3040-SELECT-PENALTY-PCT
               PERFORM 3050-AGE-LAST-MAINT
               PERFORM 3060-SET-DRAW-PERIOD
               PERFORM 3070-CHECK-MATURITY
               PERFORM 3080-SELECT-MARGIN
               PERFORM 3090-SET-TRAN
               PERFORM 3100-LIMIT-DRAW
               PERFORM 3110-EDIT-CONTACT
               PERFORM 3120-FLAG-ITEM
               PERFORM 3130-SELECT-PENALTY-PCT
               PERFORM 3140-ASSESS-AUTO-RENEW
               PERFORM 3150-SPLIT-INTEREST
               PERFORM 3160-AGE-OPEN
               PERFORM 3170-SPLIT-BALANCE
               PERFORM 3180-AGE-CLOSE
               PERFORM 3190-ASSESS-OVERRIDE
               PERFORM 3200-ACCRUE-ANNUAL-FEE
               PERFORM 3210-DESCRIBE-CHANNEL
               PERFORM 3220-SET-CHANNEL
               PERFORM 3230-COUNT-EXCEPT
               PERFORM 3240-ASSESS-HOLD
               PERFORM 3250-SET-REGION
               PERFORM 3260-ASSESS-NOTICE-SENT
               PERFORM 3270-LIMIT-ITEM
               PERFORM 3280-DESCRIBE-REASON
               PERFORM 3290-DESCRIBE-REASON
               PERFORM 3300-COUNT-RETRY
               PERFORM 3310-SET-DRAW-PERIOD
               PERFORM 3320-COUNT-SEQ
               PERFORM 3330-DESCRIBE-STATUS
               PERFORM 3340-SELECT-DISCOUNT
               PERFORM 3350-PRORATE-ANNUAL-FEE
               PERFORM 3360-EDIT-OPERATOR
               PERFORM 3370-ACCRUE-PENALTY-PCT
               PERFORM 3380-ACCUM-CHANNEL
               PERFORM 3390-LIMIT-DRAW
               PERFORM 3400-FLAG-LIMIT
               PERFORM 3410-CHECK-EFF
               PERFORM 3420-SELECT-DISCOUNT
               PERFORM 3430-SPLIT-LIMIT
               PERFORM 3440-PRORATE-BALANCE
               PERFORM 3450-PRORATE-DRAW
               PERFORM 3460-LIMIT-INTEREST
               PERFORM 3470-CHECK-EFFECTIVE
               PERFORM 3480-PRORATE-MIN-PAY
               PERFORM 3490-SET-REGION
               PERFORM 3500-AGE-CLOSE
               PERFORM 3510-ASSESS-NOTICE-SENT
               PERFORM 3520-SET-TRAN
               PERFORM 3530-PRORATE-ANNUAL-FEE
               PERFORM 3540-SET-LIEN-POS
               PERFORM 3550-ACCUM-CHANNEL
               PERFORM 3560-PRORATE-APPRAISE
               PERFORM 3570-EDIT-NAME
               PERFORM 3580-ACCRUE-LIMIT
               PERFORM 3590-ACCUM-CHANNEL
               PERFORM 3600-SET-REGION
               PERFORM 3610-AGE-CLOSE
               PERFORM 3620-AGE-EFF
               PERFORM 3630-ASSESS-SUSPEND
               PERFORM 3640-DESCRIBE-TYPE
               PERFORM 3650-DESCRIBE-CHANNEL
               PERFORM 3660-AGE-MATURITY
               PERFORM 3670-SPLIT-INTEREST
               PERFORM 3680-ACCUM-CHANNEL
               PERFORM 3690-SET-LIEN-POS
               PERFORM 3700-ACCRUE-BALANCE
               PERFORM 3710-AGE-LAST-PAY
               PERFORM 3720-COUNT-RETRY
               PERFORM 3730-CHECK-NEXT-REVIEW
               PERFORM 3740-ACCRUE-APPRAISE
               PERFORM 3750-CHECK-NEXT-REVIEW
               PERFORM 3760-DESCRIBE-TRAN
               PERFORM 3770-PRORATE-APPRAISE
               PERFORM 3780-SELECT-DISCOUNT
               PERFORM 3790-CHECK-EXPIRY
               PERFORM 3800-EDIT-DESC
               PERFORM 3810-LIMIT-ANNUAL-FEE
               PERFORM 3820-CHECK-LAST-PAY
               PERFORM 3830-ACCRUE-BALANCE
               PERFORM 3840-CHECK-EXPIRY
               PERFORM 3850-LIMIT-ANNUAL-FEE
               PERFORM 3860-EDIT-ADDR
               PERFORM 3870-AGE-NEXT-DUE
               PERFORM 3880-SET-REGION
               PERFORM 3890-CHECK-LAST-MAINT
               PERFORM 3900-AGE-EFF
               PERFORM 3910-PRORATE-ANNUAL-FEE
               PERFORM 3920-CHECK-POST
               PERFORM 3930-SELECT-DISCOUNT
               PERFORM 3940-ACCUM-DRAW-PERIOD
               PERFORM 3950-DESCRIBE-CHANNEL
               PERFORM 3960-DESCRIBE-REASON
               PERFORM 3970-ACCUM-REASON
               PERFORM 3980-SPLIT-DRAW
               PERFORM 3990-ACCUM-TRAN
               PERFORM 4000-EDIT-REF-NO
               PERFORM 4010-SPLIT-LIMIT
               PERFORM 4020-SET-TYPE
               PERFORM 4030-LIMIT-DRAW
               PERFORM 4040-SPLIT-DRAW
               PERFORM 4050-LIMIT-APPRAISE
               PERFORM 4060-AGE-NEXT-DUE
               PERFORM 4070-SELECT-MARGIN
               PERFORM 4080-LIMIT-INTEREST
               PERFORM 4090-PRORATE-ANNUAL-FEE
               PERFORM 4100-COUNT-PAYMENT
               PERFORM 4110-CHECK-NEXT-REVIEW
               PERFORM 4120-CHECK-EFF
               PERFORM 4130-LIMIT-LIMIT
               PERFORM 4140-CHECK-OPEN
               PERFORM 4150-ASSESS-HOLD
               PERFORM 4160-ACCUM-LIEN-POS
               PERFORM 4170-LIMIT-DRAW
               PERFORM 4180-SPLIT-DRAW
               PERFORM 4190-AGE-EFF
               PERFORM 4200-CHECK-NEXT-REVIEW
               PERFORM 4210-PRORATE-APPRAISE
               PERFORM 4220-ACCRUE-LIMIT
               PERFORM 4230-LIMIT-APPRAISE
               PERFORM 4240-FLAG-INTEREST
               PERFORM 4250-SELECT-PENALTY-PCT
               PERFORM 4260-FLAG-DRAW
               PERFORM 4270-ACCUM-REASON
               PERFORM 4280-SPLIT-ANNUAL-FEE
               PERFORM 4290-DESCRIBE-CHANNEL
               PERFORM 4300-CHECK-NEXT-REVIEW
               PERFORM 4310-AGE-NEXT-REVIEW
               PERFORM 4320-DESCRIBE-TRAN
               PERFORM 4330-FLAG-ITEM
               PERFORM 4340-SPLIT-LIMIT
               PERFORM 4350-FLAG-MIN-PAY
               PERFORM 4360-FLAG-ITEM
               PERFORM 4370-EDIT-REF-NO
               PERFORM 4380-SPLIT-INTEREST
               PERFORM 4390-DESCRIBE-REASON
               PERFORM 4400-SET-TYPE
               PERFORM 4410-SPLIT-LIMIT
               PERFORM 4420-ACCRUE-ANNUAL-FEE
               PERFORM 4430-CHECK-OPEN
               PERFORM 4440-SET-TRAN
               PERFORM 4450-CHECK-NEXT-DUE
               PERFORM 4460-COUNT-NOTICE
               PERFORM 4470-SELECT-MARGIN
               PERFORM 4480-EDIT-DESC
               PERFORM 4490-FLAG-APPRAISE
               PERFORM 4500-EDIT-NAME
               PERFORM 4510-ASSESS-PREFERRED
               PERFORM 4520-CHECK-NEXT-REVIEW
               PERFORM 4530-ACCRUE-ANNUAL-FEE
               PERFORM 4540-CHECK-EFF
               PERFORM 4550-AGE-LAST-MAINT
               PERFORM 4560-CHECK-EFFECTIVE
               PERFORM 4570-LIMIT-APPRAISE
               PERFORM 4580-ASSESS-PREFERRED
               PERFORM 4590-SPLIT-DRAW
               PERFORM 4600-PRORATE-ANNUAL-FEE
               PERFORM 4610-AGE-EFFECTIVE
               PERFORM 4620-SET-TRAN
               PERFORM 7030-CALL-HEFEE
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
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELT-ANNUAL-FEE-AMT * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELM-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3030-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-APPRAISE-YTD NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-LAST-MAINT.
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
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN POST DATE IS PAST
      * JSU -> KXA WHEN POST DATE IS PAST
      * KXA -> VMR WHEN POST DATE IS PAST
      * VMR -> WLJ WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-MATURITY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-INTEREST-AMT NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN MATURITY DATE IS PAST
      * PY -> FZ WHEN MATURITY DATE IS PAST
      * FZ -> RC WHEN MATURITY DATE IS PAST
      * RC -> AF WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DRAW YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-DRAW.
           IF HELM-DRAW-YTD < 50.00
               MOVE 50.00 TO HELM-DRAW-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-DRAW-YTD > 5000.00
               MOVE 5000.00 TO HELM-DRAW-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E963 CONTACT REQUIRED
      * E467 DRAW YTD NEGATIVE
      * E491 DRAW YTD OVER 50000
      * E489 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CONTACT = SPACES
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN HELM-DRAW-YTD < 0
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN HELM-DRAW-YTD > 50000.00
                   MOVE 'E491' TO WS-REJECT-CODE
               WHEN HELT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3120-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELT-APPRAISE-AMT NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3140-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-AMT ROUNDED = WS-WORK-AMT-2 *
                       HELM-MARGIN / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 20.00 TO HELM-APPRAISE-AMT
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 3.00 TO HELM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-AMT ROUNDED = WS-WORK-AMT-2 *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-APPRAISE-AMT > 0 AND HELM-APPRAISE-AMT < 10.00
               MOVE 10.00 TO HELM-APPRAISE-AMT
           END-IF
           IF HELM-APPRAISE-AMT > 250.00
               MOVE 250.00 TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE HELM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-OPEN.
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
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = HELM-BALANCE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELM-BALANCE-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE YTD FEE ON THE MIN PAY YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3190-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-MIN-PAY-YTD
                       * HELM-PENALTY-PCT / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO HELM-APPRAISE-YTD
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO HELM-APPRAISE-YTD
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-YTD ROUNDED = HELM-MIN-PAY-YTD
                       * HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-APPRAISE-YTD > 0 AND HELM-APPRAISE-YTD < 10.00
               MOVE 10.00 TO HELM-APPRAISE-YTD
           END-IF
           IF HELM-APPRAISE-YTD > 50.00
               MOVE 50.00 TO HELM-APPRAISE-YTD
           END-IF
           ADD HELM-APPRAISE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-YTD * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE LIMIT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3240-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-LIMIT-YTD *
                       HELM-MARGIN / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 7.00 TO HELM-INTEREST-YTD
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 12.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-LIMIT-YTD *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 1.00
               MOVE 1.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 250.00
               MOVE 250.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN NEXT DUE DATE IS PAST
      * HKW -> HPB WHEN NEXT DUE DATE IS PAST
      * HPB -> ULJ WHEN NEXT DUE DATE IS PAST
      * ULJ -> BSD WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW YTD FEE ON THE APPRAISE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3260-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HELM-DRAW-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   COMPUTE HELM-DRAW-YTD ROUNDED = HELM-APPRAISE-AMT *
                       HELM-PENALTY-PCT / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 7.00 TO HELM-DRAW-YTD
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 15.00 TO HELM-DRAW-YTD
               WHEN OTHER
                   COMPUTE HELM-DRAW-YTD ROUNDED = HELM-APPRAISE-AMT *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-DRAW-YTD > 0 AND HELM-DRAW-YTD < 1.00
               MOVE 1.00 TO HELM-DRAW-YTD
           END-IF
           IF HELM-DRAW-YTD > 75.00
               MOVE 75.00 TO HELM-DRAW-YTD
           END-IF
           ADD HELM-DRAW-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3270-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 10
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN LAST PAY DATE IS PAST
      * JSU -> KXA WHEN LAST PAY DATE IS PAST
      * KXA -> VMR WHEN LAST PAY DATE IS PAST
      * VMR -> WLJ WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 10
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE TIER TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-TIER-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELM-MIN-PAY-YTD NOT < WS-APPRAISE-TIER-LOW(WS-TX)
                   MOVE WS-APPRAISE-TIER-RATE(WS-TX) TO HELM-DISCOUNT
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
       3350-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELT-ANNUAL-FEE-AMT
           END-IF
           ADD HELT-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E662 OPERATOR REQUIRED
      * E209 WORK AMT 2 NEGATIVE
      * E699 WORK AMT 2 OVER 50000
      * E567 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-OPERATOR = SPACES
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELM-MIN-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-CHANNEL.
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
      * DRAW AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3390-LIMIT-DRAW.
           IF HELT-DRAW-AMT < 100.00
               MOVE 100.00 TO HELT-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-DRAW-AMT > 2500.00
               MOVE 2500.00 TO HELT-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3400-FLAG-LIMIT.
           IF HELM-LIMIT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-EFF.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRAW GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-GRID-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-DRAW-GRID-LOW(WS-TX)
                   MOVE WS-DRAW-GRID-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE HELM-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       3440-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-BALANCE-YTD ROUNDED =
                   HELM-LIMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRAW AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-DRAW-AMT ROUNDED =
                   HELT-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELT-DRAW-AMT
           END-IF
           ADD HELT-DRAW-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3460-LIMIT-INTEREST.
           IF HELM-INTEREST-AMT < 50.00
               MOVE 50.00 TO HELM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-INTEREST-AMT > 50000.00
               MOVE 50000.00 TO HELM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN PAY YTD.
      *----------------------------------------------------------------*
       3480-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-MIN-PAY-YTD ROUNDED =
                   HELT-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-MIN-PAY-YTD
           END-IF
           ADD HELM-MIN-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN EFF DATE IS PAST
      * HKW -> HPB WHEN EFF DATE IS PAST
      * HPB -> ULJ WHEN EFF DATE IS PAST
      * ULJ -> BSD WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-REGION.
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
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE APPRAISE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3510-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-APPRAISE-YTD *
                       HELM-DISCOUNT / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-APPRAISE-YTD *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN EXPIRY DATE IS PAST
      * PY -> FZ WHEN EXPIRY DATE IS PAST
      * FZ -> RC WHEN EXPIRY DATE IS PAST
      * RC -> AF WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                   HELT-DRAW-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-AMT
           END-IF
           ADD HELM-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN EFF DATE IS PAST
      * PVA -> SFC WHEN EFF DATE IS PAST
      * SFC -> AXX WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-CHANNEL.
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
      * PRORATE APPRAISE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       3560-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-AMT ROUNDED =
                   HELT-APPRAISE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E281 NAME REQUIRED
      * E469 ANNUAL FEE AMT NEGATIVE
      * E466 ANNUAL FEE AMT OVER 50000
      * E569 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-NAME = SPACES
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN HELT-ANNUAL-FEE-AMT < 0
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN HELT-ANNUAL-FEE-AMT > 50000.00
                   MOVE 'E466' TO WS-REJECT-CODE
               WHEN HELM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E569' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-LIMIT.
           IF HELM-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-LIMIT-AMT * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELM-DRAW-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ANNUAL FEE AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-CHANNEL.
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
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN LAST PAY DATE IS PAST
      * HKW -> HPB WHEN LAST PAY DATE IS PAST
      * HPB -> ULJ WHEN LAST PAY DATE IS PAST
      * ULJ -> BSD WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3620-AGE-EFF.
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
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE BALANCE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3630-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-AMT ROUNDED = HELM-BALANCE-AMT
                       * HELM-DISCOUNT / 200
               WHEN HELM-TYPE-CD = 'CD'
                   MOVE 7.00 TO HELM-APPRAISE-AMT
               WHEN HELM-TYPE-CD = 'JW'
                   MOVE 20.00 TO HELM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-AMT ROUNDED = HELM-BALANCE-AMT
                       * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-APPRAISE-AMT > 0 AND HELM-APPRAISE-AMT < 5.00
               MOVE 5.00 TO HELM-APPRAISE-AMT
           END-IF
           IF HELM-APPRAISE-AMT > 250.00
               MOVE 250.00 TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-MATURITY.
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
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE HELM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-CHANNEL-CD
                   ADD HELM-APPRAISE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-APPRAISE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN MATURITY DATE IS PAST
      * PVA -> SFC WHEN MATURITY DATE IS PAST
      * SFC -> AXX WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-BALANCE.
           IF HELM-BALANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-BALANCE-AMT * HELM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HELT-ANNUAL-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-LAST-PAY.
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
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 3
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON APPRAISE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-APPRAISE.
           IF HELM-APPRAISE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-APPRAISE-YTD * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELM-MIN-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3760-DESCRIBE-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO APPRAISE YTD.
      *----------------------------------------------------------------*
       3770-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-APPRAISE-YTD
           END-IF
           ADD HELM-APPRAISE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-INTEREST-AMT NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-EXPIRY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E936 DESC REQUIRED
      * E781 APPRAISE AMT NEGATIVE
      * E564 APPRAISE AMT OVER 5000
      * E673 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT < 0
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT > 5000.00
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN HELT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 25.00
               MOVE 25.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 50000.00
               MOVE 50000.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-LAST-PAY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-BALANCE.
           IF HELM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-BALANCE-YTD * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3850-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 50.00
               MOVE 50.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 1000.00
               MOVE 1000.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E691 ADDR REQUIRED
      * E430 WORK AMT 2 NEGATIVE
      * E562 WORK AMT 2 OVER 99999
      * E042 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-ADDR = SPACES
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E042' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-NEXT-DUE.
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
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN POST DATE IS PAST
      * HKW -> HPB WHEN POST DATE IS PAST
      * HPB -> ULJ WHEN POST DATE IS PAST
      * ULJ -> BSD WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-EFF.
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
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-APPRAISE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3910-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   HELM-ANNUAL-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-POST.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-MIN-PAY-YTD NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY DRAW PERIOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-DRAW-PERIOD-CD
                   ADD HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3960-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REASON-CD
                   ADD HELM-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELT-DRAW-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELT-DRAW-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-TRAN-CD
                   ADD HELM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E156 REF NO REQUIRED
      * E439 APPRAISE YTD NEGATIVE
      * E144 APPRAISE YTD OVER 25000
      * E447 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-YTD < 0
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-YTD > 25000.00
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN HELM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = HELM-LIMIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = HELM-LIMIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE HELM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN LAST PAY DATE IS PAST
      * JW -> KV WHEN LAST PAY DATE IS PAST
      * KV -> WS WHEN LAST PAY DATE IS PAST
      * WS -> XL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-TYPE.
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
      * DRAW AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4030-LIMIT-DRAW.
           IF HELT-DRAW-AMT < 25.00
               MOVE 25.00 TO HELT-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-DRAW-AMT > 2500.00
               MOVE 2500.00 TO HELT-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELT-DRAW-AMT / 3
           COMPUTE WS-WORK-AMT-2 = HELT-DRAW-AMT - (WS-WORK-AMT * 3)
           COMPUTE HELT-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4050-LIMIT-APPRAISE.
           IF HELM-APPRAISE-AMT < 25.00
               MOVE 25.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-AMT > 2500.00
               MOVE 2500.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-NEXT-DUE.
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
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-MIN-PAY-YTD NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-INTEREST.
           IF HELM-INTEREST-YTD < 50.00
               MOVE 50.00 TO HELM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-INTEREST-YTD > 1000.00
               MOVE 1000.00 TO HELM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       4090-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELT-ANNUAL-FEE-AMT
           END-IF
           ADD HELT-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-EFF.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-LIMIT.
           IF HELM-LIMIT-AMT < 50.00
               MOVE 50.00 TO HELM-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-LIMIT-AMT > 5000.00
               MOVE 5000.00 TO HELM-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-OPEN.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MIN PAY YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4150-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-PENALTY-PCT / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY YTD BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DRAW AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4170-LIMIT-DRAW.
           IF HELM-DRAW-AMT < 10.00
               MOVE 10.00 TO HELM-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-DRAW-AMT > 5000.00
               MOVE 5000.00 TO HELM-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELT-DRAW-AMT / 3
           COMPUTE WS-WORK-AMT-2 = HELT-DRAW-AMT - (WS-WORK-AMT * 3)
           COMPUTE HELM-APPRAISE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-EFF.
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
               WHEN WS-DAYS NOT > 180
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO APPRAISE YTD.
      *----------------------------------------------------------------*
       4210-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-YTD ROUNDED =
                   HELM-LIMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-APPRAISE-YTD
           END-IF
           ADD HELM-APPRAISE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-LIMIT.
           IF HELM-LIMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-LIMIT-YTD * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-APPRAISE.
           IF HELM-APPRAISE-AMT < 50.00
               MOVE 50.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-AMT > 5000.00
               MOVE 5000.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4240-FLAG-INTEREST.
           IF HELM-INTEREST-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRAW AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4260-FLAG-DRAW.
           IF HELT-DRAW-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELT-DRAW-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4270-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HELM-ANNUAL-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HELM-ANNUAL-FEE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE HELM-MIN-PAY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4310-AGE-NEXT-REVIEW.
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
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4320-DESCRIBE-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'AF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4330-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4340-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4350-FLAG-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-MIN-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4360-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E327 REF NO REQUIRED
      * E330 WORK AMT 2 NEGATIVE
      * E900 WORK AMT 2 OVER 5000
      * E143 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E327' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN HELM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HELM-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = HELM-INTEREST-AMT - (WS-WORK-AMT * 3)
           COMPUTE HELM-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4390-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN OPEN DATE IS PAST
      * JW -> KV WHEN OPEN DATE IS PAST
      * KV -> WS WHEN OPEN DATE IS PAST
      * WS -> XL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JW' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WS' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = HELT-LIMIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELT-LIMIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-BALANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4420-ACCRUE-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELT-ANNUAL-FEE-AMT * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELM-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4430-CHECK-OPEN.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN LAST MAINT DATE IS PAST
      * PY -> FZ WHEN LAST MAINT DATE IS PAST
      * FZ -> RC WHEN LAST MAINT DATE IS PAST
      * RC -> AF WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4440-SET-TRAN.
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
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4450-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4460-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY TIER TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4470-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-TIER-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-MIN-PAY-YTD NOT < WS-MIN-PAY-TIER-LOW(WS-TX)
                   MOVE WS-MIN-PAY-TIER-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E119 DESC REQUIRED
      * E693 WORK AMT NEGATIVE
      * E872 WORK AMT OVER 25000
      * E411 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4480-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN HELM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4490-FLAG-APPRAISE.
           IF HELM-APPRAISE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E497 NAME REQUIRED
      * E262 MIN PAY YTD NEGATIVE
      * E667 MIN PAY YTD OVER 5000
      * E857 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4500-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-NAME = SPACES
                   MOVE 'E497' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD < 0
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD > 5000.00
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN HELT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE DRAW YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4510-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-DRAW-YTD *
                       HELM-PENALTY-PCT / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 3.00 TO HELM-INTEREST-YTD
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 7.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-DRAW-YTD *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 5.00
               MOVE 5.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 75.00
               MOVE 75.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4520-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4530-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-AMT * HELM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HELM-APPRAISE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4540-CHECK-EFF.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4550-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4560-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4570-LIMIT-APPRAISE.
           IF HELT-APPRAISE-AMT < 0.00
               MOVE 0.00 TO HELT-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-APPRAISE-AMT > 2500.00
               MOVE 2500.00 TO HELT-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE LIMIT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4580-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-AMT ROUNDED = HELM-LIMIT-AMT *
                       HELM-PENALTY-PCT / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO HELM-APPRAISE-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO HELM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-AMT ROUNDED = HELM-LIMIT-AMT *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-APPRAISE-AMT > 0 AND HELM-APPRAISE-AMT < 2.00
               MOVE 2.00 TO HELM-APPRAISE-AMT
           END-IF
           IF HELM-APPRAISE-AMT > 25.00
               MOVE 25.00 TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4590-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELT-DRAW-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELT-DRAW-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-MIN-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE APPRAISE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ANNUAL FEE YTD.
      *----------------------------------------------------------------*
       4600-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                   HELT-APPRAISE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-YTD
           END-IF
           ADD HELM-ANNUAL-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4610-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN EFFECTIVE DATE IS PAST
      * PY -> FZ WHEN EFFECTIVE DATE IS PAST
      * FZ -> RC WHEN EFFECTIVE DATE IS PAST
      * RC -> AF WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4620-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

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
                   MOVE HEFEE-BALANCE-YTD
                     TO HELM-BALANCE-YTD
                   MOVE HEFEE-ANNUAL-FEE-AMT
                     TO HELM-ANNUAL-FEE-AMT
                   MOVE HEFEE-PREFERRED-FLAG
                     TO HELM-PREFERRED-FLAG
                   MOVE HEFEE-NOTICE-SENT-FLAG
                     TO HELM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-HEFEE' TO ABND-PARAGRAPH
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
                   MOVE HELIM-INTEREST-YTD
                     TO HELM-INTEREST-YTD
                   MOVE HELIM-MIN-PAY-AMT
                     TO HELM-MIN-PAY-AMT
                   MOVE HELIM-AUTO-RENEW-FLAG
                     TO HELM-AUTO-RENEW-FLAG
                   MOVE HELIM-OVERRIDE-FLAG
                     TO HELM-OVERRIDE-FLAG
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
           DISPLAY 'HELSWEP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'HELSWEP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
