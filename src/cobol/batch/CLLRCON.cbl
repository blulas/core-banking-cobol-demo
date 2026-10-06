      *================================================================*
      * PROGRAM  : CLLRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COLLECTIONS QUEUE - RECONCILIATION. READS THE COL   *
      *            TRANSACTIONS, UPDATES THE COL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. DELINQUENT ACCOUNT COLLECTION   *
      *            QUEUES, PROMISES AND LETTERS                        *
      * INPUT    : COLMST (KSDS), COLTRN, BANKCTL                      *
      * OUTPUT   : COLOUT, CLLRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CLLIM, CLRISK           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-21 JLM  ORIGINAL                                       *
      * 1995-09-08 TWH  NEW COLLECTED REPORT COLUMNS REQ1316           *
      * 1995-12-04 CRP  PROMISED STATUS CODES ADDED CHG8006            *
      * 1996-01-20 PQR  PAST-DUE EXCEPTIONS TO OPS QUEUE INC8308       *
      * 1996-06-01 LMB  NEW PROMISED REPORT COLUMNS PRB9941            *
      * 1997-04-14 MKL  PROMISED FEE SCHEDULE UPDATE REQ9444           *
      * 1998-10-03 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-10 CRP  COLLECTED CUTOFF MOVED REQ4645                 *
      * 2001-04-25 JLM  NEW PROMISED REPORT COLUMNS PRB2640            *
      * 2003-08-21 RJT  PAST-DUE ROUNDING - HALF UP CHG6857            *
      * 2004-06-15 DPS  AUDIT FINDING - PROMISED CONTROL TOTALS REQ9348*
      * 2004-12-22 DPS  FEE EXCEPTIONS TO OPS QUEUE REQ5279            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLLRCON.
       AUTHOR. M K LOWE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "COLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS COLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "COLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "COLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CLLRCRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY COLMST.
       FD  TRAN-FILE.
       COPY COLTRN.
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
                                           VALUE 11.000000.
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
      * WS-PROMISED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PROMISED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000091500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000475500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000512000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000511500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000741000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001650000000375000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000128000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000564500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003900000000419500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003950000000141000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000110000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000359500'.
       01  WS-PROMISED-GRID-TBL REDEFINES WS-PROMISED-GRID-VALUES.
           05  WS-PROMISED-GRID-ROW OCCURS 12 TIMES.
               10  WS-PROMISED-GRID-LOW    PIC 9(09)V99.
               10  WS-PROMISED-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COLLECTE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COLLECTE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000848000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000498000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000211500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000622000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000510000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017650000000115000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018150000000713000'.
       01  WS-COLLECTE-GRID-TBL REDEFINES WS-COLLECTE-GRID-VALUES.
           05  WS-COLLECTE-GRID-ROW OCCURS 7 TIMES.
               10  WS-COLLECTE-GRID-LOW    PIC 9(09)V99.
               10  WS-COLLECTE-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CLLRCON  COLLECTIONS QUEUE'.
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
       COPY CLLIMP.
       COPY CLRISKP.

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
               MOVE 'COLMST' TO ABND-FILE-NAME
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
           MOVE COLT-KEY TO COLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ASSESS-OVERRIDE
               PERFORM 3010-SELECT-FEE-PCT
               PERFORM 3020-AGE-CLOSE
               PERFORM 3030-AGE-EFFECTIVE
               PERFORM 3040-SET-ACTION
               PERFORM 3050-LIMIT-PROMISED
               PERFORM 3060-SET-QUEUE
               PERFORM 3070-CHECK-NEXT-REVIEW
               PERFORM 3080-COUNT-SEQ
               PERFORM 3090-ACCUM-QUEUE
               PERFORM 3100-PRORATE-WRITEOFF
               PERFORM 3110-SPLIT-WRITEOFF
               PERFORM 3120-LIMIT-ITEM
               PERFORM 3130-CHECK-CLOSE
               PERFORM 3140-CHECK-LAST-PAY
               PERFORM 3150-CHECK-NEXT-REVIEW
               PERFORM 3160-AGE-NOTICE
               PERFORM 3170-FLAG-PROMISED
               PERFORM 3180-ACCRUE-COLLECTED
               PERFORM 3190-ASSESS-EXEMPT
               PERFORM 3200-AGE-POST
               PERFORM 3210-EDIT-CUST-ID
               PERFORM 3220-PRORATE-FEE
               PERFORM 3230-ASSESS-EXEMPT
               PERFORM 3240-ACCRUE-MARGIN
               PERFORM 3250-LIMIT-PAST-DUE
               PERFORM 3260-LIMIT-WRITEOFF
               PERFORM 3270-ASSESS-NOTICE-SENT
               PERFORM 3280-FLAG-ITEM
               PERFORM 3290-SPLIT-PROMISED
               PERFORM 3300-ACCRUE-COLLECTED
               PERFORM 3310-SELECT-FEE-PCT
               PERFORM 3320-FLAG-PROMISED
               PERFORM 3330-ASSESS-REVIEW
               PERFORM 3340-CHECK-NEXT-REVIEW
               PERFORM 3350-CHECK-EXPIRY
               PERFORM 3360-ACCUM-REGION
               PERFORM 3370-ASSESS-OVERRIDE
               PERFORM 3380-SET-ACTION
               PERFORM 3390-SELECT-FEE-PCT
               PERFORM 3400-ASSESS-NOTICE-SENT
               PERFORM 3410-CHECK-LAST-MAINT
               PERFORM 3420-CHECK-POST
               PERFORM 3430-ACCUM-TRAN
               PERFORM 3440-SET-QUEUE
               PERFORM 3450-AGE-EXPIRY
               PERFORM 3460-SPLIT-FEE
               PERFORM 3470-SPLIT-WRITEOFF
               PERFORM 3480-AGE-EFF
               PERFORM 3490-DESCRIBE-CHANNEL
               PERFORM 3500-FLAG-ITEM
               PERFORM 3510-SPLIT-FEE
               PERFORM 3520-SELECT-MARGIN
               PERFORM 3530-SELECT-MARGIN
               PERFORM 3540-DESCRIBE-TRAN
               PERFORM 3550-EDIT-CONTACT
               PERFORM 3560-ACCUM-STATUS
               PERFORM 3570-PRORATE-FEE
               PERFORM 3580-COUNT-CYCLE
               PERFORM 3590-AGE-CLOSE
               PERFORM 3600-COUNT-NOTICE
               PERFORM 3610-SET-TRAN
               PERFORM 3620-SPLIT-PAST-DUE
               PERFORM 3630-ACCUM-QUEUE
               PERFORM 3640-ACCRUE-COLLECTED
               PERFORM 3650-LIMIT-FEE
               PERFORM 3660-ACCRUE-MARGIN
               PERFORM 3670-PRORATE-PROMISED
               PERFORM 3680-ASSESS-PREFERRED
               PERFORM 3690-ACCUM-ACTION
               PERFORM 3700-AGE-NOTICE
               PERFORM 3710-PRORATE-WRITEOFF
               PERFORM 3720-ACCRUE-FEE
               PERFORM 3730-LIMIT-PAST-DUE
               PERFORM 3740-SPLIT-PROMISED
               PERFORM 3750-CHECK-EFF
               PERFORM 3760-DESCRIBE-STATUS
               PERFORM 3770-SELECT-MARGIN
               PERFORM 3780-DESCRIBE-TRAN
               PERFORM 3790-EDIT-DESC
               PERFORM 3800-SPLIT-PAST-DUE
               PERFORM 3810-EDIT-ADDR
               PERFORM 3820-SET-ACTION
               PERFORM 3830-FLAG-FEE
               PERFORM 3840-DESCRIBE-ACTION
               PERFORM 3850-FLAG-FEE
               PERFORM 3860-COUNT-RETRY
               PERFORM 3870-DESCRIBE-ACTION
               PERFORM 3880-SPLIT-PROMISED
               PERFORM 3890-AGE-EXPIRY
               PERFORM 3900-AGE-NEXT-DUE
               PERFORM 3910-SPLIT-WRITEOFF
               PERFORM 3920-AGE-OPEN
               PERFORM 3930-DESCRIBE-ACTION
               PERFORM 3940-PRORATE-WRITEOFF
               PERFORM 3950-SET-QUEUE
               PERFORM 3960-ASSESS-REVIEW
               PERFORM 3970-SELECT-MARGIN
               PERFORM 3980-LIMIT-PAST-DUE
               PERFORM 3990-DESCRIBE-REASON
               PERFORM 4000-LIMIT-COLLECTED
               PERFORM 4010-SELECT-MARGIN
               PERFORM 4020-ACCUM-CYCLE
               PERFORM 4030-SPLIT-PROMISED
               PERFORM 4040-SPLIT-WRITEOFF
               PERFORM 4050-SET-STATUS
               PERFORM 4060-CHECK-NEXT-DUE
               PERFORM 4070-DESCRIBE-CYCLE
               PERFORM 4080-ACCRUE-PAST-DUE
               PERFORM 7030-CALL-CLLIM
               PERFORM 7020-CALL-CLRISK
               REWRITE COL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'COLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM COL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3000-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLT-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   COMPUTE COLT-COLLECTED-AMT ROUNDED = COLM-FEE-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO COLT-COLLECTED-AMT
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO COLT-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLT-COLLECTED-AMT ROUNDED = COLM-FEE-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-COLLECTED-AMT > 0 AND COLT-COLLECTED-AMT < 5.00
               MOVE 5.00 TO COLT-COLLECTED-AMT
           END-IF
           IF COLT-COLLECTED-AMT > 75.00
               MOVE 75.00 TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE GRID TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLM-PROMISED-AMT NOT < WS-COLLECTE-GRID-LOW(WS-TX)
                   MOVE WS-COLLECTE-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLT-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLT-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLT-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD COLT-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN NEXT REVIEW DATE IS PAST
      * TM -> VL WHEN NEXT REVIEW DATE IS PAST
      * VL -> ZS WHEN NEXT REVIEW DATE IS PAST
      * ZS -> KJ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PROMISED AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-PROMISED.
           IF COLM-PROMISED-AMT < 50.00
               MOVE 50.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-AMT > 5000.00
               MOVE 5000.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN EXPIRY DATE IS PAST
      * ER -> HA WHEN EXPIRY DATE IS PAST
      * HA -> HW WHEN EXPIRY DATE IS PAST
      * HW -> ME WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 3
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WRITEOFF YTD.
      *----------------------------------------------------------------*
       3100-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-WRITEOFF-YTD ROUNDED =
                   COLT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-WRITEOFF-YTD
           END-IF
           ADD COLM-WRITEOFF-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-YTD / 12
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-YTD - (WS-WORK-AMT *
               12)
           COMPUTE COLT-WRITEOFF-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3120-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PROMISED YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3170-FLAG-PROMISED.
           IF COLM-PROMISED-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PROMISED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-YTD * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF FEE ON THE WRITEOFF YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3190-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLT-WRITEOFF-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLT-WRITEOFF-AMT ROUNDED = COLM-WRITEOFF-YTD
                       * COLM-MARGIN / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO COLT-WRITEOFF-AMT
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO COLT-WRITEOFF-AMT
               WHEN OTHER
                   COMPUTE COLT-WRITEOFF-AMT ROUNDED = COLM-WRITEOFF-YTD
                       * COLM-MARGIN / 100
           END-EVALUATE
           IF COLT-WRITEOFF-AMT > 0 AND COLT-WRITEOFF-AMT < 5.00
               MOVE 5.00 TO COLT-WRITEOFF-AMT
           END-IF
           IF COLT-WRITEOFF-AMT > 250.00
               MOVE 250.00 TO COLT-WRITEOFF-AMT
           END-IF
           ADD COLT-WRITEOFF-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF COLT-POST-DATE > 0 AND COLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E542 CUST ID REQUIRED
      * E378 WORK AMT NEGATIVE
      * E439 WORK AMT OVER 50000
      * E940 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CUST-ID = SPACES
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN COLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-AMT ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED YTD FEE ON THE PAST DUE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLM-COLLECTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-PAST-DUE-YTD * COLM-FEE-PCT / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 20.00 TO COLM-COLLECTED-YTD
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 10.00 TO COLM-COLLECTED-YTD
               WHEN OTHER
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-PAST-DUE-YTD * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-COLLECTED-YTD > 0 AND COLM-COLLECTED-YTD < 10.00
               MOVE 10.00 TO COLM-COLLECTED-YTD
           END-IF
           IF COLM-COLLECTED-YTD > 250.00
               MOVE 250.00 TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLM-PROMISED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-YTD < 50.00
               MOVE 50.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-YTD > 1000.00
               MOVE 1000.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-WRITEOFF.
           IF COLM-WRITEOFF-YTD < 10.00
               MOVE 10.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-WRITEOFF-YTD > 5000.00
               MOVE 5000.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED FEE ON THE PAST DUE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3270-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-AMT ROUNDED = COLT-PAST-DUE-AMT
                       * COLM-PENALTY-PCT / 200
               WHEN COLM-REGION-CD = 'AT'
                   MOVE 20.00 TO COLM-PROMISED-AMT
               WHEN COLM-REGION-CD = 'AU'
                   MOVE 20.00 TO COLM-PROMISED-AMT
               WHEN OTHER
                   COMPUTE COLM-PROMISED-AMT ROUNDED = COLT-PAST-DUE-AMT
                       * COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLM-PROMISED-AMT > 0 AND COLM-PROMISED-AMT < 5.00
               MOVE 5.00 TO COLM-PROMISED-AMT
           END-IF
           IF COLM-PROMISED-AMT > 75.00
               MOVE 75.00 TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3280-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PROMISED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3290-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = COLM-PROMISED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLM-PROMISED-AMT - (WS-WORK-AMT * 2)
           COMPUTE COLT-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-COLLECTED.
           IF COLT-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-COLLECTED-AMT * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE GRID TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLT-PAST-DUE-AMT NOT < WS-COLLECTE-GRID-LOW(WS-TX)
                   MOVE WS-COLLECTE-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3320-FLAG-PROMISED.
           IF COLM-PROMISED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PROMISED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE PAST DUE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3330-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE COLM-FEE-YTD ROUNDED = COLM-PAST-DUE-AMT *
                       COLM-MARGIN / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO COLM-FEE-YTD
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO COLM-FEE-YTD
               WHEN OTHER
                   COMPUTE COLM-FEE-YTD ROUNDED = COLM-PAST-DUE-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-FEE-YTD > 0 AND COLM-FEE-YTD < 5.00
               MOVE 5.00 TO COLM-FEE-YTD
           END-IF
           IF COLM-FEE-YTD > 25.00
               MOVE 25.00 TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF COLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED YTD FEE ON THE PROMISED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3370-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLM-COLLECTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-PROMISED-AMT * COLM-FEE-PCT / 200
               WHEN COLM-REASON-CD = 'AT'
                   MOVE 5.00 TO COLM-COLLECTED-YTD
               WHEN COLM-REASON-CD = 'BJ'
                   MOVE 20.00 TO COLM-COLLECTED-YTD
               WHEN OTHER
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-PROMISED-AMT * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-COLLECTED-YTD > 0 AND COLM-COLLECTED-YTD < 10.00
               MOVE 10.00 TO COLM-COLLECTED-YTD
           END-IF
           IF COLM-COLLECTED-YTD > 75.00
               MOVE 75.00 TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN EFF DATE IS PAST
      * TM -> VL WHEN EFF DATE IS PAST
      * VL -> ZS WHEN EFF DATE IS PAST
      * ZS -> KJ WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED GRID TIER FOR THE WRITEOFF AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF COLT-WRITEOFF-AMT NOT < WS-PROMISED-GRID-LOW(WS-TX)
                   MOVE WS-PROMISED-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE FEE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3400-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   COMPUTE COLM-FEE-YTD ROUNDED = COLT-FEE-AMT *
                       COLM-MARGIN / 200
               WHEN COLM-REASON-CD = 'AT'
                   MOVE 3.00 TO COLM-FEE-YTD
               WHEN COLM-REASON-CD = 'BJ'
                   MOVE 10.00 TO COLM-FEE-YTD
               WHEN OTHER
                   COMPUTE COLM-FEE-YTD ROUNDED = COLT-FEE-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-FEE-YTD > 0 AND COLM-FEE-YTD < 2.00
               MOVE 2.00 TO COLM-FEE-YTD
           END-IF
           IF COLM-FEE-YTD > 100.00
               MOVE 100.00 TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF COLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN NEXT REVIEW DATE IS PAST
      * ER -> HA WHEN NEXT REVIEW DATE IS PAST
      * HA -> HW WHEN NEXT REVIEW DATE IS PAST
      * HW -> ME WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLT-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-EFF-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3500-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLT-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLM-FEE-YTD NOT < WS-COLLECTE-GRID-LOW(WS-TX)
                   MOVE WS-COLLECTE-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED GRID TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF COLM-PROMISED-AMT NOT < WS-PROMISED-GRID-LOW(WS-TX)
                   MOVE WS-PROMISED-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WO'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E601 CONTACT REQUIRED
      * E826 PAST DUE YTD NEGATIVE
      * E543 PAST DUE YTD OVER 99999
      * E602 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD < 0
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD > 99999.00
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN COLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-STATUS-CD
                   ADD COLM-PAST-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PAST-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-FEE-AMT ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN OPEN DATE IS PAST
      * CL -> LT WHEN OPEN DATE IS PAST
      * LT -> WO WHEN OPEN DATE IS PAST
      * WO -> RF WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CL'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LT'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WO' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WO'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLT-PAST-DUE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLT-PAST-DUE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLT-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-COLLECTED.
           IF COLT-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-COLLECTED-AMT * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-FEE.
           IF COLM-FEE-AMT < 10.00
               MOVE 10.00 TO COLM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-FEE-AMT > 5000.00
               MOVE 5000.00 TO COLM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PROMISED YTD.
      *----------------------------------------------------------------*
       3670-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PROMISED-YTD ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PROMISED-YTD
           END-IF
           ADD COLM-PROMISED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED FEE ON THE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3680-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-AMT ROUNDED = COLM-FEE-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLM-CYCLE-CD = 'BE'
                   MOVE 3.00 TO COLM-PROMISED-AMT
               WHEN COLM-CYCLE-CD = 'GV'
                   MOVE 20.00 TO COLM-PROMISED-AMT
               WHEN OTHER
                   COMPUTE COLM-PROMISED-AMT ROUNDED = COLM-FEE-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PROMISED-AMT > 0 AND COLM-PROMISED-AMT < 10.00
               MOVE 10.00 TO COLM-PROMISED-AMT
           END-IF
           IF COLM-PROMISED-AMT > 35.00
               MOVE 35.00 TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY ACTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-ACTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-ACTION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-ACTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WRITEOFF AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-WRITEOFF-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-WRITEOFF-AMT
           END-IF
           ADD COLT-WRITEOFF-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-FEE.
           IF COLT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-FEE-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLM-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-PAST-DUE.
           IF COLT-PAST-DUE-AMT < 0.00
               MOVE 0.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-PAST-DUE-AMT > 50000.00
               MOVE 50000.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PROMISED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3740-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = COLM-PROMISED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = COLM-PROMISED-AMT - (WS-WORK-AMT * 4)
           COMPUTE COLT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF COLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3760-DESCRIBE-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLT-FEE-AMT NOT < WS-COLLECTE-GRID-LOW(WS-TX)
                   MOVE WS-COLLECTE-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WO'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E184 DESC REQUIRED
      * E094 PROMISED YTD NEGATIVE
      * E230 PROMISED YTD OVER 250000
      * E930 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E184' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD < 0
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD > 250000.00
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E930' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLT-PAST-DUE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLT-PAST-DUE-AMT - (WS-WORK-AMT * 2)
           COMPUTE COLM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E127 ADDR REQUIRED
      * E571 WORK AMT 2 NEGATIVE
      * E745 WORK AMT 2 OVER 50000
      * E777 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN COLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN EXPIRY DATE IS PAST
      * TM -> VL WHEN EXPIRY DATE IS PAST
      * VL -> ZS WHEN EXPIRY DATE IS PAST
      * ZS -> KJ WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3830-FLAG-FEE.
           IF COLM-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3850-FLAG-FEE.
           IF COLT-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE COLM-PROMISED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-AMT / 4
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-AMT - (WS-WORK-AMT * 4)
           COMPUTE COLM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WRITEOFF AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-WRITEOFF-AMT ROUNDED =
                   COLT-WRITEOFF-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-WRITEOFF-AMT
           END-IF
           ADD COLM-WRITEOFF-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN LAST MAINT DATE IS PAST
      * ER -> HA WHEN LAST MAINT DATE IS PAST
      * HA -> HW WHEN LAST MAINT DATE IS PAST
      * HW -> ME WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE YTD FEE ON THE PROMISED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3960-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLM-PROMISED-AMT
                       * COLM-MARGIN / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 12.00 TO COLM-PAST-DUE-YTD
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 5.00 TO COLM-PAST-DUE-YTD
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLM-PROMISED-AMT
                       * COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-PAST-DUE-YTD > 0 AND COLM-PAST-DUE-YTD < 5.00
               MOVE 5.00 TO COLM-PAST-DUE-YTD
           END-IF
           IF COLM-PAST-DUE-YTD > 100.00
               MOVE 100.00 TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE GRID TIER FOR THE WRITEOFF AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLT-WRITEOFF-AMT NOT < WS-COLLECTE-GRID-LOW(WS-TX)
                   MOVE WS-COLLECTE-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-PAST-DUE.
           IF COLT-PAST-DUE-AMT < 10.00
               MOVE 10.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-PAST-DUE-AMT > 2500.00
               MOVE 2500.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4000-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 0.00
               MOVE 0.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 50000.00
               MOVE 50000.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE GRID TIER FOR THE WRITEOFF AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLM-WRITEOFF-AMT NOT < WS-COLLECTE-GRID-LOW(WS-TX)
                   MOVE WS-COLLECTE-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PROMISED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = COLM-PROMISED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLM-PROMISED-AMT - (WS-WORK-AMT * 2)
           COMPUTE COLM-COLLECTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-YTD / 3
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-YTD - (WS-WORK-AMT * 3)
           COMPUTE COLM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN POST DATE IS PAST
      * UR -> XM WHEN POST DATE IS PAST
      * XM -> YA WHEN POST DATE IS PAST
      * YA -> PS WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-YTD * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE COL FIELDS TO CLLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-CLLIM.
           INITIALIZE CLLIM-PARMS
           MOVE COLM-COLLECTED-YTD
             TO CLLIM-COLLECTED-YTD
           MOVE COLM-COLLECTED-AMT
             TO CLLIM-COLLECTED-AMT
           MOVE COLM-PAST-DUE-AMT
             TO CLLIM-PAST-DUE-AMT
           MOVE COLM-PROMISED-YTD
             TO CLLIM-PROMISED-YTD
           MOVE COLM-WRITEOFF-YTD
             TO CLLIM-WRITEOFF-YTD
           MOVE COLM-FEE-YTD
             TO CLLIM-FEE-YTD
           MOVE COLM-CLOSE-DATE
             TO CLLIM-CLOSE-DATE
           MOVE COLM-NOTICE-DATE
             TO CLLIM-NOTICE-DATE
           MOVE COLM-OPEN-DATE
             TO CLLIM-OPEN-DATE
           MOVE COLM-LAST-PAY-DATE
             TO CLLIM-LAST-PAY-DATE
           MOVE COLM-CYCLE-CD
             TO CLLIM-CYCLE-CD
           MOVE COLM-REASON-CD
             TO CLLIM-REASON-CD
           MOVE COLM-QUEUE-CD
             TO CLLIM-QUEUE-CD
           MOVE COLM-REGION-CD
             TO CLLIM-REGION-CD
           MOVE COLM-REVIEW-FLAG
             TO CLLIM-REVIEW-FLAG
           MOVE COLM-OVERRIDE-FLAG
             TO CLLIM-OVERRIDE-FLAG
           MOVE COLM-PREFERRED-FLAG
             TO CLLIM-PREFERRED-FLAG
           MOVE COLM-ITEM-CNT
             TO CLLIM-ITEM-CNT
           MOVE COLM-NOTICE-CNT
             TO CLLIM-NOTICE-CNT
           MOVE COLM-FEE-PCT
             TO CLLIM-FEE-PCT
           MOVE COLM-MARGIN
             TO CLLIM-MARGIN
           MOVE COLM-CONTACT
             TO CLLIM-CONTACT
           MOVE COLM-NAME
             TO CLLIM-NAME
           CALL 'CLLIM' USING CLLIM-PARMS
           EVALUATE CLLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLLIM-COLLECTED-YTD
                     TO COLM-COLLECTED-YTD
                   MOVE CLLIM-COLLECTED-AMT
                     TO COLM-COLLECTED-AMT
                   MOVE CLLIM-PAST-DUE-AMT
                     TO COLM-PAST-DUE-AMT
                   MOVE CLLIM-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   MOVE CLLIM-FEE-YTD
                     TO COLM-FEE-YTD
                   MOVE CLLIM-REVIEW-FLAG
                     TO COLM-REVIEW-FLAG
                   MOVE CLLIM-OVERRIDE-FLAG
                     TO COLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CLLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE COL FIELDS TO CLRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-CLRISK.
           INITIALIZE CLRISK-PARMS
           MOVE COLM-PAST-DUE-AMT
             TO CLRISK-PAST-DUE-AMT
           MOVE COLM-PAST-DUE-YTD
             TO CLRISK-PAST-DUE-YTD
           MOVE COLM-COLLECTED-AMT
             TO CLRISK-COLLECTED-AMT
           MOVE COLM-WRITEOFF-YTD
             TO CLRISK-WRITEOFF-YTD
           MOVE COLM-FEE-YTD
             TO CLRISK-FEE-YTD
           MOVE COLM-PROMISED-YTD
             TO CLRISK-PROMISED-YTD
           MOVE COLM-EFFECTIVE-DATE
             TO CLRISK-EFFECTIVE-DATE
           MOVE COLM-OPEN-DATE
             TO CLRISK-OPEN-DATE
           MOVE COLM-LAST-MAINT-DATE
             TO CLRISK-LAST-MAINT-DATE
           MOVE COLM-NOTICE-DATE
             TO CLRISK-NOTICE-DATE
           MOVE COLM-CYCLE-CD
             TO CLRISK-CYCLE-CD
           MOVE COLM-REGION-CD
             TO CLRISK-REGION-CD
           MOVE COLM-ACTION-CD
             TO CLRISK-ACTION-CD
           MOVE COLM-STATUS-CD
             TO CLRISK-STATUS-CD
           MOVE COLM-NOTICE-SENT-FLAG
             TO CLRISK-NOTICE-SENT-FLAG
           MOVE COLM-REVIEW-FLAG
             TO CLRISK-REVIEW-FLAG
           MOVE COLM-EXEMPT-FLAG
             TO CLRISK-EXEMPT-FLAG
           MOVE COLM-ITEM-CNT
             TO CLRISK-ITEM-CNT
           MOVE COLM-RETRY-CNT
             TO CLRISK-RETRY-CNT
           MOVE COLM-FEE-PCT
             TO CLRISK-FEE-PCT
           MOVE COLM-MARGIN
             TO CLRISK-MARGIN
           MOVE COLM-DESC
             TO CLRISK-DESC
           MOVE COLM-CONTACT
             TO CLRISK-CONTACT
           CALL 'CLRISK' USING CLRISK-PARMS
           EVALUATE CLRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLRISK-PAST-DUE-AMT
                     TO COLM-PAST-DUE-AMT
                   MOVE CLRISK-FEE-YTD
                     TO COLM-FEE-YTD
                   MOVE CLRISK-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   MOVE CLRISK-NOTICE-SENT-FLAG
                     TO COLM-NOTICE-SENT-FLAG
                   MOVE CLRISK-EXEMPT-FLAG
                     TO COLM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CLRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE COLT-KEY TO RD-KEY
           MOVE COLT-TRAN-CD TO RD-CODE
           MOVE COLT-COLLECTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE COLT-KEY TO RD-KEY
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
           DISPLAY 'CLLRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CLLRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
