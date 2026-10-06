      *================================================================*
      * PROGRAM  : SDBCONV                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAFE DEPOSIT BOXES - FILE CONVERSION. READS THE SDB *
      *            TRANSACTIONS, UPDATES THE SDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. SAFE DEPOSIT BOX RENTAL BILLING,*
      *            ACCESS LOG AND DRILLING                             *
      * INPUT    : SDBMST (KSDS), SDBTRN, BANKCTL                      *
      * OUTPUT   : SDBOUT, SDBCORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SDWAIV, SDLIM           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-12 JLM  ORIGINAL                                       *
      * 1994-11-16 LMB  PERFORMANCE - PREPAID READ AHEAD INC1760       *
      * 1996-01-05 CRP  PREPAID TABLE EXPANDED PRB6931                 *
      * 1996-07-18 JLM  LATE-FEE ROUNDING - HALF UP REQ1737            *
      * 1997-07-02 LMB  LATE-FEE STATUS CODES ADDED INC3670            *
      * 1997-06-23 JLM  RENT CUTOFF MOVED PRB1412                      *
      * 2002-07-23 EWS  PERFORMANCE - PREPAID READ AHEAD CHG1116       *
      * 2004-07-04 JLM  PERFORMANCE - KEY-FEE READ AHEAD INC9828       *
      * 2007-02-23 GAF  KEY-FEE ROUNDING - HALF UP INC0336             *
      * 2009-02-25 JLM  PERFORMANCE - PREPAID READ AHEAD PRB0857       *
      * 2009-03-05 GAF  PREPAID ROUNDING - HALF UP CHG7839             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDBCONV.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "SDBMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SDBM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "SDBTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "SDBOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "SDBCORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY SDBMST.
       FD  TRAN-FILE.
       COPY SDBTRN.
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
      * WS-DEPOSIT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000763000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000855500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000657000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000515500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000379000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000386500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017300000000867000'.
       01  WS-DEPOSIT-TIER-TBL REDEFINES WS-DEPOSIT-TIER-VALUES.
           05  WS-DEPOSIT-TIER-ROW OCCURS 7 TIMES.
               10  WS-DEPOSIT-TIER-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LATE-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LATE-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000832000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000208000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000136000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000035000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000385500'.
       01  WS-LATE-FEE-BAND-TBL REDEFINES WS-LATE-FEE-BAND-VALUES.
           05  WS-LATE-FEE-BAND-ROW OCCURS 5 TIMES.
               10  WS-LATE-FEE-BAND-LOW    PIC 9(09)V99.
               10  WS-LATE-FEE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DRILL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRILL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000582500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000068000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000242500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000446500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000549500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000735500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000066000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022350000000349000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023350000000022500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023450000000416000'.
       01  WS-DRILL-SCHED-TBL REDEFINES WS-DRILL-SCHED-VALUES.
           05  WS-DRILL-SCHED-ROW OCCURS 10 TIMES.
               10  WS-DRILL-SCHED-LOW      PIC 9(09)V99.
               10  WS-DRILL-SCHED-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SDBCONV  SAFE DEPOSIT BOXES'.
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
       COPY SDWAIVP.
       COPY SDLIMP.

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
               MOVE 'SDBMST' TO ABND-FILE-NAME
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
           MOVE SDBT-KEY TO SDBM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-EDIT-ADDR
               PERFORM 3010-AGE-NEXT-REVIEW
               PERFORM 3020-ACCRUE-LATE-FEE
               PERFORM 3030-SET-REASON
               PERFORM 3040-PRORATE-RENT
               PERFORM 3050-LIMIT-LATE-FEE
               PERFORM 3060-CHECK-NEXT-DUE
               PERFORM 3070-SELECT-FEE-PCT
               PERFORM 3080-SPLIT-RENT
               PERFORM 3090-AGE-LAST-STMT
               PERFORM 3100-SELECT-FEE-PCT
               PERFORM 3110-DESCRIBE-SOURCE
               PERFORM 3120-AGE-NEXT-DUE
               PERFORM 3130-LIMIT-ITEM
               PERFORM 3140-EDIT-CONTACT
               PERFORM 3150-SET-VAULT
               PERFORM 3160-SET-TRAN
               PERFORM 3170-FLAG-LATE-FEE
               PERFORM 3180-SPLIT-KEY-FEE
               PERFORM 3190-ACCRUE-KEY-FEE
               PERFORM 3200-CHECK-CLOSE
               PERFORM 3210-SET-VAULT
               PERFORM 3220-LIMIT-DEPOSIT
               PERFORM 3230-ASSESS-EXEMPT
               PERFORM 3240-DESCRIBE-VAULT
               PERFORM 3250-LIMIT-DEPOSIT
               PERFORM 3260-ASSESS-SUSPEND
               PERFORM 3270-DESCRIBE-VAULT
               PERFORM 3280-PRORATE-LATE-FEE
               PERFORM 3290-SELECT-DISCOUNT
               PERFORM 3300-ACCRUE-LATE-FEE
               PERFORM 3310-PRORATE-PREPAID
               PERFORM 3320-SPLIT-LATE-FEE
               PERFORM 3330-ACCRUE-KEY-FEE
               PERFORM 3340-SELECT-PENALTY-PCT
               PERFORM 3350-CHECK-NEXT-DUE
               PERFORM 3360-LIMIT-DEPOSIT
               PERFORM 3370-ASSESS-REVIEW
               PERFORM 3380-SPLIT-PREPAID
               PERFORM 3390-ACCRUE-PREPAID
               PERFORM 3400-ASSESS-SUSPEND
               PERFORM 3410-DESCRIBE-VAULT
               PERFORM 3420-EDIT-REJECT
               PERFORM 3430-LIMIT-KEY-FEE
               PERFORM 3440-AGE-CLOSE
               PERFORM 3450-DESCRIBE-TRAN
               PERFORM 3460-EDIT-DESC
               PERFORM 3470-ACCUM-CHANNEL
               PERFORM 3480-SET-BRANCH
               PERFORM 3490-COUNT-SEQ
               PERFORM 3500-SET-BOX-SIZE
               PERFORM 3510-ACCRUE-PENALTY-PCT
               PERFORM 3520-DESCRIBE-CHANNEL
               PERFORM 3530-PRORATE-DEPOSIT
               PERFORM 3540-ACCRUE-PREPAID
               PERFORM 3550-SELECT-FEE-PCT
               PERFORM 3560-COUNT-SEQ
               PERFORM 3570-SELECT-FEE-PCT
               PERFORM 3580-ACCUM-VAULT
               PERFORM 3590-DESCRIBE-BOX-SIZE
               PERFORM 3600-ACCRUE-DEPOSIT
               PERFORM 3610-FLAG-KEY-FEE
               PERFORM 3620-SELECT-PENALTY-PCT
               PERFORM 3630-CHECK-MATURITY
               PERFORM 3640-SET-REASON
               PERFORM 3650-SELECT-FEE-PCT
               PERFORM 3660-SET-SOURCE
               PERFORM 3670-PRORATE-DEPOSIT
               PERFORM 3680-SELECT-DISCOUNT
               PERFORM 3690-ACCRUE-FEE-PCT
               PERFORM 3700-PRORATE-LATE-FEE
               PERFORM 3710-ACCRUE-DEPOSIT
               PERFORM 3720-ACCUM-BOX-SIZE
               PERFORM 3730-COUNT-NOTICE
               PERFORM 3740-ACCRUE-DEPOSIT
               PERFORM 3750-SET-BOX-SIZE
               PERFORM 3760-ASSESS-PREFERRED
               PERFORM 3770-PRORATE-RENT
               PERFORM 3780-FLAG-KEY-FEE
               PERFORM 3790-LIMIT-DEPOSIT
               PERFORM 3800-COUNT-PAYMENT
               PERFORM 3810-LIMIT-DRILL
               PERFORM 3820-AGE-LAST-MAINT
               PERFORM 3830-COUNT-RETRY
               PERFORM 3840-DESCRIBE-REASON
               PERFORM 3850-AGE-LAST-ACT
               PERFORM 3860-CHECK-LAST-ACT
               PERFORM 3870-ACCRUE-DRILL
               PERFORM 3880-PRORATE-KEY-FEE
               PERFORM 3890-PRORATE-RENT
               PERFORM 3900-ASSESS-AUTO-RENEW
               PERFORM 3910-FLAG-DEPOSIT
               PERFORM 3920-ACCRUE-PREPAID
               PERFORM 3930-ACCRUE-DRILL
               PERFORM 3940-ACCRUE-DEPOSIT
               PERFORM 3950-ASSESS-PREFERRED
               PERFORM 3960-COUNT-ITEM
               PERFORM 3970-AGE-NOTICE
               PERFORM 3980-CHECK-NEXT-DUE
               PERFORM 3990-ASSESS-PREFERRED
               PERFORM 4000-ACCUM-BOX-SIZE
               PERFORM 4010-ACCRUE-PENALTY-PCT
               PERFORM 4020-ACCUM-BRANCH
               PERFORM 4030-ACCUM-BOX-SIZE
               PERFORM 4040-ASSESS-PREFERRED
               PERFORM 4050-FLAG-RENT
               PERFORM 4060-PRORATE-DRILL
               PERFORM 4070-ASSESS-PREFERRED
               PERFORM 4080-SPLIT-KEY-FEE
               PERFORM 4090-COUNT-SEQ
               PERFORM 4100-EDIT-MEMO
               PERFORM 4110-EDIT-REJECT
               PERFORM 4120-PRORATE-PREPAID
               PERFORM 4130-DESCRIBE-CHANNEL
               PERFORM 4140-EDIT-REF-NO
               PERFORM 4150-DESCRIBE-VAULT
               PERFORM 4160-FLAG-ITEM
               PERFORM 4170-AGE-LAST-MAINT
               PERFORM 4180-DESCRIBE-TRAN
               PERFORM 4190-SELECT-PENALTY-PCT
               PERFORM 4200-EDIT-OPERATOR
               PERFORM 4210-SELECT-FEE-PCT
               PERFORM 4220-FLAG-DRILL
               PERFORM 4230-SELECT-DISCOUNT
               PERFORM 4240-SPLIT-KEY-FEE
               PERFORM 4250-DESCRIBE-CYCLE
               PERFORM 4260-ACCUM-BRANCH
               PERFORM 4270-ASSESS-EXEMPT
               PERFORM 4280-COUNT-SEQ
               PERFORM 4290-SPLIT-DEPOSIT
               PERFORM 4300-ASSESS-REVIEW
               PERFORM 7040-CALL-SDWAIV
               PERFORM 7020-CALL-SDLIM
               REWRITE SDB-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'SDBMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM SDB-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E519 ADDR REQUIRED
      * E532 LATE FEE AMT NEGATIVE
      * E365 LATE FEE AMT OVER 250000
      * E319 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-ADDR = SPACES
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-AMT < 0
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-AMT > 250000.00
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN SDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-REVIEW-DATE > 0 AND SDBM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-YTD * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN NEXT REVIEW DATE IS PAST
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> S WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-RENT-AMT ROUNDED =
                   SDBM-KEY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LATE FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-LATE-FEE.
           IF SDBM-LATE-FEE-AMT < 0.00
               MOVE 0.00 TO SDBM-LATE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-LATE-FEE-AMT > 1000.00
               MOVE 1000.00 TO SDBM-LATE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRILL SCHED TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBT-DEPOSIT-AMT NOT < WS-DRILL-SCHED-LOW(WS-TX)
                   MOVE WS-DRILL-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBT-RENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBT-RENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE SDBM-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBM-DEPOSIT-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E359 CONTACT REQUIRED
      * E872 KEY FEE AMT NEGATIVE
      * E140 KEY FEE AMT OVER 5000
      * E834 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-AMT < 0
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-AMT > 5000.00
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN OPEN DATE IS PAST
      * ET -> FF WHEN OPEN DATE IS PAST
      * FF -> NG WHEN OPEN DATE IS PAST
      * NG -> NH WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN LAST STMT DATE IS PAST
      * AC -> DR WHEN LAST STMT DATE IS PAST
      * DR -> KY WHEN LAST STMT DATE IS PAST
      * KY -> CL WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3170-FLAG-LATE-FEE.
           IF SDBM-LATE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBM-KEY-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-KEY-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-KEY-FEE.
           IF SDBT-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-KEY-FEE-AMT * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST STMT DATE IS PAST
      * ET -> FF WHEN LAST STMT DATE IS PAST
      * FF -> NG WHEN LAST STMT DATE IS PAST
      * NG -> NH WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3220-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-YTD > 500.00
               MOVE 500.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE LATE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-LATE-FEE-YTD
                       * SDBM-DISCOUNT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 20.00 TO SDBM-DEPOSIT-AMT
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 20.00 TO SDBM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-LATE-FEE-YTD
                       * SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-AMT > 0 AND SDBM-DEPOSIT-AMT < 1.00
               MOVE 1.00 TO SDBM-DEPOSIT-AMT
           END-IF
           IF SDBM-DEPOSIT-AMT > 100.00
               MOVE 100.00 TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ET'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-DEPOSIT.
           IF SDBT-DEPOSIT-AMT < 25.00
               MOVE 25.00 TO SDBT-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-DEPOSIT-AMT > 50000.00
               MOVE 50000.00 TO SDBT-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3260-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-DRILL-AMT ROUNDED = SDBT-DEPOSIT-AMT *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 7.00 TO SDBM-DRILL-AMT
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 3.00 TO SDBM-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDBM-DRILL-AMT ROUNDED = SDBT-DEPOSIT-AMT *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-DRILL-AMT > 0 AND SDBM-DRILL-AMT < 5.00
               MOVE 5.00 TO SDBM-DRILL-AMT
           END-IF
           IF SDBM-DRILL-AMT > 50.00
               MOVE 50.00 TO SDBM-DRILL-AMT
           END-IF
           ADD SDBM-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ET'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-LATE-FEE-AMT ROUNDED =
                   SDBM-RENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-LATE-FEE-AMT
           END-IF
           ADD SDBM-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE BAND TIER FOR THE DRILL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SDBT-DRILL-AMT NOT < WS-LATE-FEE-BAND-LOW(WS-TX)
                   MOVE WS-LATE-FEE-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-YTD * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-PREPAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PREPAID AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-AMT ROUNDED =
                   SDBT-DRILL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-PREPAID-AMT
           END-IF
           ADD SDBM-PREPAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDBM-LATE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBM-LATE-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBT-DEPOSIT-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-AMT < 50.00
               MOVE 50.00 TO SDBM-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-AMT > 50000.00
               MOVE 50000.00 TO SDBM-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE YTD FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3370-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-KEY-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDBM-KEY-FEE-YTD ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-FEE-PCT / 200
               WHEN SDBT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO SDBM-KEY-FEE-YTD
               WHEN SDBT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO SDBM-KEY-FEE-YTD
               WHEN OTHER
                   COMPUTE SDBM-KEY-FEE-YTD ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-KEY-FEE-YTD > 0 AND SDBM-KEY-FEE-YTD < 2.00
               MOVE 2.00 TO SDBM-KEY-FEE-YTD
           END-IF
           IF SDBM-KEY-FEE-YTD > 25.00
               MOVE 25.00 TO SDBM-KEY-FEE-YTD
           END-IF
           ADD SDBM-KEY-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PREPAID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-PREPAID.
           COMPUTE WS-WORK-AMT = SDBM-PREPAID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-PREPAID-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3400-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBM-RENT-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 12.00 TO SDBM-RENT-AMT
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 7.00 TO SDBM-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBM-RENT-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-AMT > 0 AND SDBM-RENT-AMT < 5.00
               MOVE 5.00 TO SDBM-RENT-AMT
           END-IF
           IF SDBM-RENT-AMT > 50.00
               MOVE 50.00 TO SDBM-RENT-AMT
           END-IF
           ADD SDBM-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ET'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E038 REJECT CD REQUIRED
      * E995 LATE FEE YTD NEGATIVE
      * E614 LATE FEE YTD OVER 5000
      * E878 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-REJECT-CD = SPACES
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD < 0
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD > 5000.00
                   MOVE 'E614' TO WS-REJECT-CODE
               WHEN SDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-KEY-FEE.
           IF SDBM-KEY-FEE-AMT < 25.00
               MOVE 25.00 TO SDBM-KEY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-KEY-FEE-AMT > 1000.00
               MOVE 1000.00 TO SDBM-KEY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E949 DESC REQUIRED
      * E366 PREPAID AMT NEGATIVE
      * E644 PREPAID AMT OVER 50000
      * E804 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-DESC = SPACES
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-AMT < 0
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-AMT > 50000.00
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E804' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-CHANNEL-CD
                   ADD SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN NEXT DUE DATE IS PAST
      * FJW -> YFB WHEN NEXT DUE DATE IS PAST
      * YFB -> EXS WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 12
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST STMT DATE IS PAST
      * MG -> VG WHEN LAST STMT DATE IS PAST
      * VG -> VN WHEN LAST STMT DATE IS PAST
      * VN -> XJ WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-KEY-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBM-KEY-FEE-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBM-LATE-FEE-YTD NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-DEPOSIT.
           IF SDBT-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-DEPOSIT-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3610-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRILL SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-SCHED-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-DRILL-SCHED-LOW(WS-TX)
                   MOVE WS-DRILL-SCHED-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDBM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN CLOSE DATE IS PAST
      * K -> L WHEN CLOSE DATE IS PAST
      * L -> M WHEN CLOSE DATE IS PAST
      * M -> S WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRILL SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-DRILL-SCHED-LOW(WS-TX)
                   MOVE WS-DRILL-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN LAST ACT DATE IS PAST
      * JZ -> PS WHEN LAST ACT DATE IS PAST
      * PS -> UM WHEN LAST ACT DATE IS PAST
      * UM -> DD WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       3670-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-YTD ROUNDED =
                   SDBT-DEPOSIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-YTD
           END-IF
           ADD SDBM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE BAND TIER FOR THE RENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SDBM-RENT-YTD NOT < WS-LATE-FEE-BAND-LOW(WS-TX)
                   MOVE WS-LATE-FEE-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       3700-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-LATE-FEE-YTD ROUNDED =
                   SDBM-DEPOSIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-DEPOSIT.
           IF SDBT-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-DEPOSIT-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-DEPOSIT.
           IF SDBM-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DEPOSIT-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST ACT DATE IS PAST
      * MG -> VG WHEN LAST ACT DATE IS PAST
      * VG -> VN WHEN LAST ACT DATE IS PAST
      * VN -> XJ WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3760-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDBM-RENT-AMT ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-SOURCE-CD = 'DD'
                   MOVE 5.00 TO SDBM-RENT-AMT
               WHEN SDBM-SOURCE-CD = 'JZ'
                   MOVE 7.00 TO SDBM-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBM-RENT-AMT ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-AMT > 0 AND SDBM-RENT-AMT < 1.00
               MOVE 1.00 TO SDBM-RENT-AMT
           END-IF
           IF SDBM-RENT-AMT > 250.00
               MOVE 250.00 TO SDBM-RENT-AMT
           END-IF
           ADD SDBM-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-RENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * KEY FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3780-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-YTD > 1000.00
               MOVE 1000.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-PAYMENT.
           IF SDBM-PAYMENT-CNT < 9999999
               ADD 1 TO SDBM-PAYMENT-CNT
           END-IF
           IF SDBM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3810-LIMIT-DRILL.
           IF SDBT-DRILL-AMT < 25.00
               MOVE 25.00 TO SDBT-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-DRILL-AMT > 500.00
               MOVE 500.00 TO SDBT-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 5
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-DRILL.
           IF SDBM-DRILL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-YTD * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBT-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3880-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SDBM-KEY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT YTD.
      *----------------------------------------------------------------*
       3890-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-RENT-YTD ROUNDED =
                   SDBM-DEPOSIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3900-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-YTD ROUNDED = SDBM-DEPOSIT-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 3.00 TO SDBM-DEPOSIT-YTD
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 3.00 TO SDBM-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-YTD ROUNDED = SDBM-DEPOSIT-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-YTD > 0 AND SDBM-DEPOSIT-YTD < 2.00
               MOVE 2.00 TO SDBM-DEPOSIT-YTD
           END-IF
           IF SDBM-DEPOSIT-YTD > 35.00
               MOVE 35.00 TO SDBM-DEPOSIT-YTD
           END-IF
           ADD SDBM-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3910-FLAG-DEPOSIT.
           IF SDBM-DEPOSIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-DRILL.
           IF SDBT-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-DRILL-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-DEPOSIT.
           IF SDBM-DEPOSIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DEPOSIT-AMT * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-PREPAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE FEE ON THE RENT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3950-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBM-KEY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBM-KEY-FEE-AMT ROUNDED = SDBM-RENT-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-SOURCE-CD = 'DD'
                   MOVE 5.00 TO SDBM-KEY-FEE-AMT
               WHEN SDBM-SOURCE-CD = 'JZ'
                   MOVE 7.00 TO SDBM-KEY-FEE-AMT
               WHEN OTHER
                   COMPUTE SDBM-KEY-FEE-AMT ROUNDED = SDBM-RENT-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-KEY-FEE-AMT > 0 AND SDBM-KEY-FEE-AMT < 10.00
               MOVE 10.00 TO SDBM-KEY-FEE-AMT
           END-IF
           IF SDBM-KEY-FEE-AMT > 150.00
               MOVE 150.00 TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3960-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 10
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3990-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-YTD ROUNDED = SDBM-DEPOSIT-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 15.00 TO SDBM-DEPOSIT-YTD
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 20.00 TO SDBM-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-YTD ROUNDED = SDBM-DEPOSIT-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-YTD > 0 AND SDBM-DEPOSIT-YTD < 1.00
               MOVE 1.00 TO SDBM-DEPOSIT-YTD
           END-IF
           IF SDBM-DEPOSIT-YTD > 75.00
               MOVE 75.00 TO SDBM-DEPOSIT-YTD
           END-IF
           ADD SDBM-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BRANCH-CD
                   ADD SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4040-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-FEE-PCT / 200
               WHEN SDBT-TRAN-CD = 'RN'
                   MOVE 10.00 TO SDBM-DEPOSIT-AMT
               WHEN SDBT-TRAN-CD = 'AC'
                   MOVE 7.00 TO SDBM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-AMT > 0 AND SDBM-DEPOSIT-AMT < 2.00
               MOVE 2.00 TO SDBM-DEPOSIT-AMT
           END-IF
           IF SDBM-DEPOSIT-AMT > 75.00
               MOVE 75.00 TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4050-FLAG-RENT.
           IF SDBM-RENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE FEE ON THE LATE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4070-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBM-KEY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBM-KEY-FEE-AMT ROUNDED = SDBM-LATE-FEE-YTD
                       * SDBM-DISCOUNT / 200
               WHEN SDBM-CYCLE-CD = 'AP'
                   MOVE 10.00 TO SDBM-KEY-FEE-AMT
               WHEN SDBM-CYCLE-CD = 'DA'
                   MOVE 5.00 TO SDBM-KEY-FEE-AMT
               WHEN OTHER
                   COMPUTE SDBM-KEY-FEE-AMT ROUNDED = SDBM-LATE-FEE-YTD
                       * SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-KEY-FEE-AMT > 0 AND SDBM-KEY-FEE-AMT < 10.00
               MOVE 10.00 TO SDBM-KEY-FEE-AMT
           END-IF
           IF SDBM-KEY-FEE-AMT > 75.00
               MOVE 75.00 TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBT-KEY-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-KEY-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 6
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E437 MEMO REQUIRED
      * E543 RENT AMT NEGATIVE
      * E260 RENT AMT OVER 25000
      * E718 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-MEMO = SPACES
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN SDBT-RENT-AMT < 0
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN SDBT-RENT-AMT > 25000.00
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E791 REJECT CD REQUIRED
      * E636 RENT YTD NEGATIVE
      * E344 RENT YTD OVER 10000
      * E207 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-REJECT-CD = SPACES
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD < 0
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD > 10000.00
                   MOVE 'E344' TO WS-REJECT-CODE
               WHEN SDBM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PREPAID AMT.
      *----------------------------------------------------------------*
       4120-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-AMT ROUNDED =
                   SDBT-KEY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-PREPAID-AMT
           END-IF
           ADD SDBM-PREPAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E004 REF NO REQUIRED
      * E496 LATE FEE AMT NEGATIVE
      * E553 LATE FEE AMT OVER 5000
      * E983 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-AMT < 0
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-AMT > 5000.00
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ET'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE DRILL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBT-DRILL-AMT NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E851 OPERATOR REQUIRED
      * E374 KEY FEE AMT NEGATIVE
      * E221 KEY FEE AMT OVER 25000
      * E631 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-OPERATOR = SPACES
                   MOVE 'E851' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-AMT < 0
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-AMT > 25000.00
                   MOVE 'E221' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-DRILL.
           IF SDBT-DRILL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBT-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT TIER TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-TIER-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBM-DEPOSIT-YTD NOT < WS-DEPOSIT-TIER-LOW(WS-TX)
                   MOVE WS-DEPOSIT-TIER-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4240-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBM-KEY-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-KEY-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BRANCH-CD
                   ADD SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4270-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 7.00 TO SDBM-DEPOSIT-AMT
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 5.00 TO SDBM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-AMT > 0 AND SDBM-DEPOSIT-AMT < 2.00
               MOVE 2.00 TO SDBM-DEPOSIT-AMT
           END-IF
           IF SDBM-DEPOSIT-AMT > 250.00
               MOVE 250.00 TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4280-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 6
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBM-DEPOSIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBM-DEPOSIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE SDBM-KEY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL FEE ON THE KEY FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4300-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBT-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDBT-DRILL-AMT ROUNDED = SDBT-KEY-FEE-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 10.00 TO SDBT-DRILL-AMT
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 3.00 TO SDBT-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDBT-DRILL-AMT ROUNDED = SDBT-KEY-FEE-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBT-DRILL-AMT > 0 AND SDBT-DRILL-AMT < 2.00
               MOVE 2.00 TO SDBT-DRILL-AMT
           END-IF
           IF SDBT-DRILL-AMT > 250.00
               MOVE 250.00 TO SDBT-DRILL-AMT
           END-IF
           ADD SDBT-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-SDWAIV.
           INITIALIZE SDWAIV-PARMS
           MOVE SDBM-DEPOSIT-YTD
             TO SDWAIV-DEPOSIT-YTD
           MOVE SDBM-DRILL-AMT
             TO SDWAIV-DRILL-AMT
           MOVE SDBM-RENT-YTD
             TO SDWAIV-RENT-YTD
           MOVE SDBM-LATE-FEE-AMT
             TO SDWAIV-LATE-FEE-AMT
           MOVE SDBM-DRILL-YTD
             TO SDWAIV-DRILL-YTD
           MOVE SDBM-KEY-FEE-YTD
             TO SDWAIV-KEY-FEE-YTD
           MOVE SDBM-NEXT-DUE-DATE
             TO SDWAIV-NEXT-DUE-DATE
           MOVE SDBM-OPEN-DATE
             TO SDWAIV-OPEN-DATE
           MOVE SDBM-MATURITY-DATE
             TO SDWAIV-MATURITY-DATE
           MOVE SDBM-CLOSE-DATE
             TO SDWAIV-CLOSE-DATE
           MOVE SDBM-VAULT-CD
             TO SDWAIV-VAULT-CD
           MOVE SDBM-SOURCE-CD
             TO SDWAIV-SOURCE-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDWAIV-BOX-SIZE-CD
           MOVE SDBM-BRANCH-CD
             TO SDWAIV-BRANCH-CD
           MOVE SDBM-SUSPEND-FLAG
             TO SDWAIV-SUSPEND-FLAG
           MOVE SDBM-REVIEW-FLAG
             TO SDWAIV-REVIEW-FLAG
           MOVE SDBM-OVERRIDE-FLAG
             TO SDWAIV-OVERRIDE-FLAG
           MOVE SDBM-PAYMENT-CNT
             TO SDWAIV-PAYMENT-CNT
           MOVE SDBM-NOTICE-CNT
             TO SDWAIV-NOTICE-CNT
           MOVE SDBM-PENALTY-PCT
             TO SDWAIV-PENALTY-PCT
           MOVE SDBM-FEE-PCT
             TO SDWAIV-FEE-PCT
           MOVE SDBM-CUST-ID
             TO SDWAIV-CUST-ID
           MOVE SDBM-NAME
             TO SDWAIV-NAME
           CALL 'SDWAIV' USING SDWAIV-PARMS
           EVALUATE SDWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDWAIV-DEPOSIT-YTD
                     TO SDBM-DEPOSIT-YTD
                   MOVE SDWAIV-DRILL-AMT
                     TO SDBM-DRILL-AMT
                   MOVE SDWAIV-LATE-FEE-AMT
                     TO SDBM-LATE-FEE-AMT
                   MOVE SDWAIV-KEY-FEE-YTD
                     TO SDBM-KEY-FEE-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-SDWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-SDLIM.
           INITIALIZE SDLIM-PARMS
           MOVE SDBM-RENT-YTD
             TO SDLIM-RENT-YTD
           MOVE SDBM-DEPOSIT-YTD
             TO SDLIM-DEPOSIT-YTD
           MOVE SDBM-LATE-FEE-YTD
             TO SDLIM-LATE-FEE-YTD
           MOVE SDBM-DEPOSIT-AMT
             TO SDLIM-DEPOSIT-AMT
           MOVE SDBM-LATE-FEE-AMT
             TO SDLIM-LATE-FEE-AMT
           MOVE SDBM-DRILL-YTD
             TO SDLIM-DRILL-YTD
           MOVE SDBM-LAST-ACT-DATE
             TO SDLIM-LAST-ACT-DATE
           MOVE SDBM-OPEN-DATE
             TO SDLIM-OPEN-DATE
           MOVE SDBM-NEXT-DUE-DATE
             TO SDLIM-NEXT-DUE-DATE
           MOVE SDBM-NOTICE-DATE
             TO SDLIM-NOTICE-DATE
           MOVE SDBM-CYCLE-CD
             TO SDLIM-CYCLE-CD
           MOVE SDBM-BRANCH-CD
             TO SDLIM-BRANCH-CD
           MOVE SDBM-REASON-CD
             TO SDLIM-REASON-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDLIM-BOX-SIZE-CD
           MOVE SDBM-OVERRIDE-FLAG
             TO SDLIM-OVERRIDE-FLAG
           MOVE SDBM-EXEMPT-FLAG
             TO SDLIM-EXEMPT-FLAG
           MOVE SDBM-REVIEW-FLAG
             TO SDLIM-REVIEW-FLAG
           MOVE SDBM-RETRY-CNT
             TO SDLIM-RETRY-CNT
           MOVE SDBM-NOTICE-CNT
             TO SDLIM-NOTICE-CNT
           MOVE SDBM-DISCOUNT
             TO SDLIM-DISCOUNT
           MOVE SDBM-FEE-PCT
             TO SDLIM-FEE-PCT
           MOVE SDBM-CONTACT
             TO SDLIM-CONTACT
           MOVE SDBM-ADDR
             TO SDLIM-ADDR
           CALL 'SDLIM' USING SDLIM-PARMS
           EVALUATE SDLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDLIM-DEPOSIT-YTD
                     TO SDBM-DEPOSIT-YTD
                   MOVE SDLIM-DEPOSIT-AMT
                     TO SDBM-DEPOSIT-AMT
                   MOVE SDLIM-LATE-FEE-AMT
                     TO SDBM-LATE-FEE-AMT
                   MOVE SDLIM-OVERRIDE-FLAG
                     TO SDBM-OVERRIDE-FLAG
                   MOVE SDLIM-REVIEW-FLAG
                     TO SDBM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SDLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE SDBT-KEY TO RD-KEY
           MOVE SDBT-TRAN-CD TO RD-CODE
           MOVE SDBT-KEY-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE SDBT-KEY TO RD-KEY
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
           DISPLAY 'SDBCONV READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SDBCONV' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
