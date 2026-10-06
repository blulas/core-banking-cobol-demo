      *================================================================*
      * PROGRAM  : HELEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : HOME EQUITY LINES - NIGHTLY EXTRACT. READS THE HEL  *
      *            TRANSACTIONS, UPDATES THE HEL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOME EQUITY LINE DRAWS, BILLING,*
      *            RATE CHANGE AND FREEZE                              *
      * INPUT    : HELMST (KSDS), HELTRN, BANKCTL                      *
      * OUTPUT   : HELOUT, HELEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, HERISK                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-05 DPS  ORIGINAL                                       *
      * 1993-06-23 GAF  APPRAISE RERUN PROTECTION CHG8583              *
      * 1994-09-10 SKN  APPRAISE EXCEPTIONS TO OPS QUEUE INC7915       *
      * 1997-12-09 BHS  LIMIT STATUS CODES ADDED PRB4300               *
      * 1997-11-20 BHS  ANNUAL-FEE RERUN PROTECTION CHG6882            *
      * 1998-04-05 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-05 SKN  DRAW EXCEPTIONS TO OPS QUEUE CHG4248           *
      * 2005-11-17 CRP  MIN-PAY ROUNDING - HALF UP REQ9984             *
      * 2007-11-16 LMB  LIMIT LIMIT REVISED REQ7040                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELEXTR.
       AUTHOR. C R PATEL.

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
                                           VALUE 20.000000.
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
      * WS-APPRAISE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-APPRAISE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000817500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000533000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000694500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000416000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000862000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001850000000468000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000441500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000333000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000839500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004200000000460000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004300000000306000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004550000000328500'.
       01  WS-APPRAISE-GRID-TBL REDEFINES WS-APPRAISE-GRID-VALUES.
           05  WS-APPRAISE-GRID-ROW OCCURS 12 TIMES.
               10  WS-APPRAISE-GRID-LOW    PIC 9(09)V99.
               10  WS-APPRAISE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ANNUAL-F-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ANNUAL-F-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000444000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000689500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000693000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000246000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000840500'.
       01  WS-ANNUAL-F-GRID-TBL REDEFINES WS-ANNUAL-F-GRID-VALUES.
           05  WS-ANNUAL-F-GRID-ROW OCCURS 6 TIMES.
               10  WS-ANNUAL-F-GRID-LOW    PIC 9(09)V99.
               10  WS-ANNUAL-F-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  HELEXTR  HOME EQUITY LINES'.
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
               PERFORM 3000-AGE-MATURITY
               PERFORM 3010-SELECT-PENALTY-PCT
               PERFORM 3020-FLAG-APPRAISE
               PERFORM 3030-FLAG-MIN-PAY
               PERFORM 3040-SET-TRAN
               PERFORM 3050-AGE-CLOSE
               PERFORM 3060-ASSESS-PREFERRED
               PERFORM 3070-SELECT-PENALTY-PCT
               PERFORM 3080-AGE-OPEN
               PERFORM 3090-DESCRIBE-DRAW-PERIOD
               PERFORM 3100-ACCUM-REGION
               PERFORM 3110-SELECT-PENALTY-PCT
               PERFORM 3120-CHECK-NEXT-REVIEW
               PERFORM 3130-SPLIT-APPRAISE
               PERFORM 3140-PRORATE-ANNUAL-FEE
               PERFORM 3150-ASSESS-AUTO-RENEW
               PERFORM 3160-ASSESS-AUTO-RENEW
               PERFORM 3170-AGE-OPEN
               PERFORM 3180-CHECK-NEXT-DUE
               PERFORM 3190-SET-TYPE
               PERFORM 3200-EDIT-NAME
               PERFORM 3210-SET-TRAN
               PERFORM 3220-SET-REGION
               PERFORM 3230-CHECK-EFFECTIVE
               PERFORM 3240-SPLIT-INTEREST
               PERFORM 3250-DESCRIBE-TRAN
               PERFORM 3260-AGE-LAST-MAINT
               PERFORM 3270-LIMIT-APPRAISE
               PERFORM 3280-ACCRUE-ANNUAL-FEE
               PERFORM 3290-COUNT-PAYMENT
               PERFORM 3300-FLAG-INTEREST
               PERFORM 3310-EDIT-OPERATOR
               PERFORM 3320-EDIT-OPERATOR
               PERFORM 3330-DESCRIBE-STATUS
               PERFORM 3340-PRORATE-LIMIT
               PERFORM 3350-PRORATE-APPRAISE
               PERFORM 3360-CHECK-NEXT-REVIEW
               PERFORM 3370-SELECT-MARGIN
               PERFORM 3380-SELECT-MARGIN
               PERFORM 3390-FLAG-DRAW
               PERFORM 3400-EDIT-MEMO
               PERFORM 3410-SPLIT-APPRAISE
               PERFORM 3420-ACCUM-CHANNEL
               PERFORM 3430-LIMIT-DRAW
               PERFORM 3440-DESCRIBE-LIEN-POS
               PERFORM 3450-CHECK-EFF
               PERFORM 3460-ASSESS-HOLD
               PERFORM 3470-PRORATE-ANNUAL-FEE
               PERFORM 3480-AGE-EXPIRY
               PERFORM 3490-ACCUM-REASON
               PERFORM 3500-COUNT-NOTICE
               PERFORM 3510-DESCRIBE-TRAN
               PERFORM 3520-LIMIT-ANNUAL-FEE
               PERFORM 3530-ACCRUE-BALANCE
               PERFORM 3540-SET-REASON
               PERFORM 3550-DESCRIBE-CHANNEL
               PERFORM 3560-ASSESS-AUTO-RENEW
               PERFORM 3570-ACCUM-REASON
               PERFORM 3580-ACCUM-LIEN-POS
               PERFORM 3590-FLAG-APPRAISE
               PERFORM 3600-EDIT-ADDR
               PERFORM 3610-SELECT-PENALTY-PCT
               PERFORM 3620-CHECK-NEXT-REVIEW
               PERFORM 3630-PRORATE-LIMIT
               PERFORM 3640-PRORATE-DRAW
               PERFORM 3650-ASSESS-REVERSAL
               PERFORM 3660-LIMIT-BALANCE
               PERFORM 3670-AGE-NEXT-REVIEW
               PERFORM 3680-SPLIT-INTEREST
               PERFORM 3690-FLAG-DRAW
               PERFORM 3700-DESCRIBE-DRAW-PERIOD
               PERFORM 3710-LIMIT-DRAW
               PERFORM 3720-ASSESS-AUTO-RENEW
               PERFORM 3730-DESCRIBE-DRAW-PERIOD
               PERFORM 3740-ASSESS-NOTICE-SENT
               PERFORM 3750-CHECK-NEXT-REVIEW
               PERFORM 3760-PRORATE-LIMIT
               PERFORM 3770-ACCUM-TYPE
               PERFORM 3780-CHECK-LAST-PAY
               PERFORM 3790-CHECK-POST
               PERFORM 3800-CHECK-LAST-PAY
               PERFORM 3810-AGE-NEXT-DUE
               PERFORM 3820-ASSESS-HOLD
               PERFORM 3830-ACCRUE-INTEREST
               PERFORM 3840-AGE-LAST-PAY
               PERFORM 3850-LIMIT-ITEM
               PERFORM 3860-CHECK-EFFECTIVE
               PERFORM 3870-EDIT-ADDR
               PERFORM 3880-LIMIT-APPRAISE
               PERFORM 3890-AGE-MATURITY
               PERFORM 3900-SET-REGION
               PERFORM 3910-CHECK-NEXT-DUE
               PERFORM 3920-SET-CHANNEL
               PERFORM 3930-CHECK-CLOSE
               PERFORM 3940-SPLIT-LIMIT
               PERFORM 3950-CHECK-NEXT-REVIEW
               PERFORM 3960-ASSESS-REVERSAL
               PERFORM 3970-ACCUM-REGION
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
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 90
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE GRID TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-GRID-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HELM-BALANCE-YTD NOT < WS-APPRAISE-GRID-LOW(WS-TX)
                   MOVE WS-APPRAISE-GRID-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3020-FLAG-APPRAISE.
           IF HELM-APPRAISE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3030-FLAG-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-MIN-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN LAST MAINT DATE IS PAST
      * PY -> FZ WHEN LAST MAINT DATE IS PAST
      * FZ -> RC WHEN LAST MAINT DATE IS PAST
      * RC -> AF WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-TRAN.
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
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-CLOSE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3060-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-INTEREST-AMT
                       * HELM-DISCOUNT / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 12.00 TO HELM-INTEREST-YTD
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 15.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-INTEREST-AMT
                       * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 10.00
               MOVE 10.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 75.00
               MOVE 75.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F GRID TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-GRID-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELM-APPRAISE-AMT NOT < WS-ANNUAL-F-GRID-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-GRID-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-OPEN.
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
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REGION-CD
                   ADD HELM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE GRID TIER FOR THE MIN PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-GRID-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HELM-MIN-PAY-AMT NOT < WS-APPRAISE-GRID-LOW(WS-TX)
                   MOVE WS-APPRAISE-GRID-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELM-APPRAISE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HELM-APPRAISE-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                   HELM-DRAW-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-AMT
           END-IF
           ADD HELM-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW YTD FEE ON THE BALANCE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3150-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELM-DRAW-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-DRAW-YTD ROUNDED = HELM-BALANCE-AMT *
                       HELM-MARGIN / 200
               WHEN HELM-TYPE-CD = 'CD'
                   MOVE 5.00 TO HELM-DRAW-YTD
               WHEN HELM-TYPE-CD = 'JW'
                   MOVE 15.00 TO HELM-DRAW-YTD
               WHEN OTHER
                   COMPUTE HELM-DRAW-YTD ROUNDED = HELM-BALANCE-AMT *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-DRAW-YTD > 0 AND HELM-DRAW-YTD < 1.00
               MOVE 1.00 TO HELM-DRAW-YTD
           END-IF
           IF HELM-DRAW-YTD > 25.00
               MOVE 25.00 TO HELM-DRAW-YTD
           END-IF
           ADD HELM-DRAW-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE YTD FEE ON THE MIN PAY YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3160-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELM-ANNUAL-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                       HELM-MIN-PAY-YTD * HELM-DISCOUNT / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 7.00 TO HELM-ANNUAL-FEE-YTD
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 3.00 TO HELM-ANNUAL-FEE-YTD
               WHEN OTHER
                   COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                       HELM-MIN-PAY-YTD * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-ANNUAL-FEE-YTD > 0 AND HELM-ANNUAL-FEE-YTD < 2.00
               MOVE 2.00 TO HELM-ANNUAL-FEE-YTD
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 50.00
               MOVE 50.00 TO HELM-ANNUAL-FEE-YTD
           END-IF
           ADD HELM-ANNUAL-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-OPEN.
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
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-MIN-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN EFFECTIVE DATE IS PAST
      * JW -> KV WHEN EFFECTIVE DATE IS PAST
      * KV -> WS WHEN EFFECTIVE DATE IS PAST
      * WS -> XL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JW' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WS' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E761 NAME REQUIRED
      * E132 LIMIT YTD NEGATIVE
      * E393 LIMIT YTD OVER 50000
      * E167 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-NAME = SPACES
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN HELM-LIMIT-YTD < 0
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN HELM-LIMIT-YTD > 50000.00
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN HELT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN OPEN DATE IS PAST
      * PY -> FZ WHEN OPEN DATE IS PAST
      * FZ -> RC WHEN OPEN DATE IS PAST
      * RC -> AF WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-TRAN.
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
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN LAST MAINT DATE IS PAST
      * HKW -> HPB WHEN LAST MAINT DATE IS PAST
      * HPB -> ULJ WHEN LAST MAINT DATE IS PAST
      * ULJ -> BSD WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HELM-INTEREST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELM-INTEREST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE HELM-DRAW-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-LAST-MAINT.
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
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3270-LIMIT-APPRAISE.
           IF HELT-APPRAISE-AMT < 10.00
               MOVE 10.00 TO HELT-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-APPRAISE-AMT > 1000.00
               MOVE 1000.00 TO HELT-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-AMT * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HELM-DRAW-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3300-FLAG-INTEREST.
           IF HELM-INTEREST-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E144 OPERATOR REQUIRED
      * E067 APPRAISE YTD NEGATIVE
      * E871 APPRAISE YTD OVER 50000
      * E880 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-OPERATOR = SPACES
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-YTD < 0
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-YTD > 50000.00
                   MOVE 'E871' TO WS-REJECT-CODE
               WHEN HELM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E880' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E209 OPERATOR REQUIRED
      * E277 ANNUAL FEE AMT NEGATIVE
      * E625 ANNUAL FEE AMT OVER 25000
      * E279 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-OPERATOR = SPACES
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN HELT-ANNUAL-FEE-AMT < 0
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN HELT-ANNUAL-FEE-AMT > 25000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E279' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3340-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-LIMIT-AMT ROUNDED =
                   HELT-DRAW-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELT-LIMIT-AMT
           END-IF
           ADD HELT-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       3350-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-AMT ROUNDED =
                   HELM-DRAW-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F GRID TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-GRID-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELM-LIMIT-AMT NOT < WS-ANNUAL-F-GRID-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-GRID-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE GRID TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-GRID-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HELM-ANNUAL-FEE-AMT NOT < WS-APPRAISE-GRID-LOW(WS-TX)
                   MOVE WS-APPRAISE-GRID-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRAW YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3390-FLAG-DRAW.
           IF HELM-DRAW-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-DRAW-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E331 MEMO REQUIRED
      * E265 DRAW AMT NEGATIVE
      * E599 DRAW AMT OVER 50000
      * E696 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT < 0
                   MOVE 'E265' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT > 50000.00
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E696' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELT-APPRAISE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELT-APPRAISE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE HELM-DRAW-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-CHANNEL-CD
                   ADD HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DRAW YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-DRAW.
           IF HELM-DRAW-YTD < 0.00
               MOVE 0.00 TO HELM-DRAW-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-DRAW-YTD > 50000.00
               MOVE 50000.00 TO HELM-DRAW-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-EFF.
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
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE MIN PAY AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3460-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-AMT ROUNDED = HELM-MIN-PAY-AMT *
                       HELM-PENALTY-PCT / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 20.00 TO HELM-BALANCE-AMT
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 10.00 TO HELM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE HELM-BALANCE-AMT ROUNDED = HELM-MIN-PAY-AMT *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-BALANCE-AMT > 0 AND HELM-BALANCE-AMT < 5.00
               MOVE 5.00 TO HELM-BALANCE-AMT
           END-IF
           IF HELM-BALANCE-AMT > 50.00
               MOVE 50.00 TO HELM-BALANCE-AMT
           END-IF
           ADD HELM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE DRAW YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ANNUAL FEE YTD.
      *----------------------------------------------------------------*
       3470-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                   HELM-DRAW-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-YTD
           END-IF
           ADD HELM-ANNUAL-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-EXPIRY.
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
      * ACCUMULATE MIN PAY AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-REASON.
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT < 10.00
               MOVE 10.00 TO HELT-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-ANNUAL-FEE-AMT > 1000.00
               MOVE 1000.00 TO HELT-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-BALANCE.
           IF HELM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-BALANCE-YTD * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN EFF DATE IS PAST
      * NWS -> ZJM WHEN EFF DATE IS PAST
      * ZJM -> KST WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-REASON.
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
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE APPRAISE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3560-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-APPRAISE-YTD *
                       HELM-DISCOUNT / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-APPRAISE-YTD *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DRAW YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REASON-CD
                   ADD HELM-DRAW-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-DRAW-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DRAW YTD BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELM-DRAW-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-DRAW-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3590-FLAG-APPRAISE.
           IF HELM-APPRAISE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E970 ADDR REQUIRED
      * E930 APPRAISE AMT NEGATIVE
      * E908 APPRAISE AMT OVER 10000
      * E504 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-ADDR = SPACES
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT < 0
                   MOVE 'E930' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT > 10000.00
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN HELM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ANNUAL F GRID TIER FOR THE ANNUAL FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ANNUAL-F-GRID-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELM-ANNUAL-FEE-YTD NOT < WS-ANNUAL-F-GRID-LOW(WS-TX)
                   MOVE WS-ANNUAL-F-GRID-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-LIMIT-AMT ROUNDED =
                   HELM-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELT-LIMIT-AMT
           END-IF
           ADD HELT-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRAW YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-DRAW-YTD ROUNDED =
                   HELM-DRAW-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-DRAW-YTD
           END-IF
           ADD HELM-DRAW-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE APPRAISE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3650-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-AMT ROUNDED = HELT-APPRAISE-AMT
                       * HELM-DISCOUNT / 200
               WHEN HELM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 15.00 TO HELM-INTEREST-AMT
               WHEN HELM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 7.00 TO HELM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE HELM-INTEREST-AMT ROUNDED = HELT-APPRAISE-AMT
                       * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-INTEREST-AMT > 0 AND HELM-INTEREST-AMT < 10.00
               MOVE 10.00 TO HELM-INTEREST-AMT
           END-IF
           IF HELM-INTEREST-AMT > 100.00
               MOVE 100.00 TO HELM-INTEREST-AMT
           END-IF
           ADD HELM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3660-LIMIT-BALANCE.
           IF HELM-BALANCE-YTD < 50.00
               MOVE 50.00 TO HELM-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-BALANCE-YTD > 50000.00
               MOVE 50000.00 TO HELM-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-NEXT-REVIEW.
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
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-DRAW-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3680-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = HELM-INTEREST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = HELM-INTEREST-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRAW AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3690-FLAG-DRAW.
           IF HELM-DRAW-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-DRAW-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DRAW AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3710-LIMIT-DRAW.
           IF HELM-DRAW-AMT < 50.00
               MOVE 50.00 TO HELM-DRAW-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-DRAW-AMT > 500.00
               MOVE 500.00 TO HELM-DRAW-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE LIMIT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3720-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-LIMIT-YTD *
                       HELM-MARGIN / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-LIMIT-YTD *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE ANNUAL FEE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3740-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-ANNUAL-FEE-AMT *
                       HELM-MARGIN / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-ANNUAL-FEE-AMT *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
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
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3760-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   HELM-LIMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-TYPE-CD
                   ADD HELM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-LAST-PAY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-POST.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-LAST-PAY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-NEXT-DUE.
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
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE APPRAISE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3820-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN HELM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO HELT-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE HELT-LIMIT-AMT ROUNDED = HELM-APPRAISE-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 3.00 TO HELT-LIMIT-AMT
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 15.00 TO HELT-LIMIT-AMT
               WHEN OTHER
                   COMPUTE HELT-LIMIT-AMT ROUNDED = HELM-APPRAISE-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELT-LIMIT-AMT > 0 AND HELT-LIMIT-AMT < 5.00
               MOVE 5.00 TO HELT-LIMIT-AMT
           END-IF
           IF HELT-LIMIT-AMT > 25.00
               MOVE 25.00 TO HELT-LIMIT-AMT
           END-IF
           ADD HELT-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-INTEREST.
           IF HELM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-INTEREST-AMT * HELM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO HELM-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-LAST-PAY.
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
               WHEN WS-DAYS NOT > 30
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-MIN-PAY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3850-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO HELM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E383 ADDR REQUIRED
      * E150 ANNUAL FEE YTD NEGATIVE
      * E017 ANNUAL FEE YTD OVER 50000
      * E497 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-ADDR = SPACES
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-YTD < 0
                   MOVE 'E150' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-YTD > 50000.00
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN HELM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E497' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-APPRAISE.
           IF HELT-APPRAISE-AMT < 100.00
               MOVE 100.00 TO HELT-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-APPRAISE-AMT > 2500.00
               MOVE 2500.00 TO HELT-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-MATURITY.
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
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN EFF DATE IS PAST
      * HKW -> HPB WHEN EFF DATE IS PAST
      * HPB -> ULJ WHEN EFF DATE IS PAST
      * ULJ -> BSD WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-REGION.
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
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-CHANNEL.
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
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-CLOSE.
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
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = HELT-LIMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELT-LIMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE HELT-ANNUAL-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MIN PAY YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3960-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-DISCOUNT / 200
               WHEN HELM-TYPE-CD = 'CD'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN HELM-TYPE-CD = 'JW'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-MIN-PAY-YTD *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-REGION.
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
                   MOVE HERISK-MIN-PAY-AMT
                     TO HELM-MIN-PAY-AMT
                   MOVE HERISK-BALANCE-YTD
                     TO HELM-BALANCE-YTD
                   MOVE HERISK-MIN-PAY-YTD
                     TO HELM-MIN-PAY-YTD
                   MOVE HERISK-HOLD-FLAG
                     TO HELM-HOLD-FLAG
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
           DISPLAY 'HELEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'HELEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
