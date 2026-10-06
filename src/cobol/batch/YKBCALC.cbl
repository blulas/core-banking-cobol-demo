      *================================================================*
      * PROGRAM  : YKBCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DATE WINDOW BRIDGE - PERIODIC CALCULATION. READS THE*
      *            YDB TRANSACTIONS, UPDATES THE YDB MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. TWO-DIGIT YEAR         *
      *            WINDOWING BRIDGE FOR CENTURY DATE CONVERSION        *
      * INPUT    : YDBMST (KSDS), YDBTRN, BANKCTL                      *
      * OUTPUT   : YDBOUT, YKBCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, YKPRC, YKTIER, YKWAIV   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-12 LMB  ORIGINAL                                       *
      * 1994-05-25 LMB  AMOUNT-2 RERUN PROTECTION CHG8213              *
      * 1995-11-01 RJT  CONTROL FEE SCHEDULE UPDATE INC2471            *
      * 1998-07-18 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-02-07 PQR  CONTROL ROUNDING - HALF UP REQ1977             *
      * 1999-12-25 PQR  AMOUNT-2 CALCULATION CORRECTED PRB9234         *
      * 1999-07-03 CRP  CONTROL ROUNDING - HALF UP REQ4436             *
      * 2002-07-04 CRP  CONTROL ROUNDING - HALF UP PRB2971             *
      * 2002-10-14 PQR  AMOUNT-1 RERUN PROTECTION INC0408              *
      * 2004-10-05 GAF  AUDIT FINDING - CONTROL CONTROL TOTALS INC3615 *
      * 2006-07-15 CRP  AMOUNT-2 EXCEPTIONS TO OPS QUEUE INC7774       *
      * 2006-07-07 SKN  AMOUNT-2 FEE SCHEDULE UPDATE INC4747           *
      * 2008-03-09 SKN  CONTROL TABLE EXPANDED PRB2760                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKBCALC.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "YDBMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS YDBM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "YDBTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "YDBOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "YKBCARPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY YDBMST.
       FD  TRAN-FILE.
       COPY YDBTRN.
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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 90.
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
      * WS-CONTROL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000573500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000799500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000525500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000502500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004100000000619500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004350000000385000'.
       01  WS-CONTROL-SCHED-TBL REDEFINES WS-CONTROL-SCHED-VALUES.
           05  WS-CONTROL-SCHED-ROW OCCURS 6 TIMES.
               10  WS-CONTROL-SCHED-LOW    PIC 9(09)V99.
               10  WS-CONTROL-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-1-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000589000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000138000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000326500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000639000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001850000000203000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002100000000595500'.
       01  WS-AMOUNT-1-GRID-TBL REDEFINES WS-AMOUNT-1-GRID-VALUES.
           05  WS-AMOUNT-1-GRID-ROW OCCURS 7 TIMES.
               10  WS-AMOUNT-1-GRID-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-1-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  YKBCALC  DATE WINDOW BRIDGE'.
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
       COPY YKPRCP.
       COPY YKTIERP.
       COPY YKWAIVP.

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
               MOVE 'YDBMST' TO ABND-FILE-NAME
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
           MOVE YDBT-KEY TO YDBM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-PIVOT
               PERFORM 3010-CHECK-LAST-ACT
               PERFORM 3020-PRORATE-AMOUNT-1
               PERFORM 3030-FLAG-AMOUNT-1
               PERFORM 3040-LIMIT-AMOUNT-2
               PERFORM 3050-DESCRIBE-TRAN
               PERFORM 3060-SET-CHANNEL
               PERFORM 3070-AGE-LAST-PAY
               PERFORM 3080-SELECT-DISCOUNT
               PERFORM 3090-ACCUM-CYCLE
               PERFORM 3100-CHECK-LAST-MAINT
               PERFORM 3110-PRORATE-AMOUNT-2
               PERFORM 3120-LIMIT-ITEM
               PERFORM 3130-DESCRIBE-TRAN
               PERFORM 3140-ACCUM-REGION
               PERFORM 3150-SET-REASON
               PERFORM 3160-FLAG-AMOUNT-2
               PERFORM 3170-SET-SOURCE
               PERFORM 3180-PRORATE-AMOUNT-1
               PERFORM 3190-COUNT-SEQ
               PERFORM 3200-ACCUM-REGION
               PERFORM 3210-SET-SOURCE
               PERFORM 3220-COUNT-PAYMENT
               PERFORM 3230-ACCRUE-AMOUNT-2
               PERFORM 3240-PRORATE-AMOUNT-1
               PERFORM 3250-COUNT-SEQ
               PERFORM 3260-SELECT-MARGIN
               PERFORM 3270-COUNT-RETRY
               PERFORM 3280-FLAG-AMOUNT-2
               PERFORM 3290-ASSESS-OVERRIDE
               PERFORM 3300-SPLIT-AMOUNT-2
               PERFORM 3310-DESCRIBE-SOURCE
               PERFORM 3320-LIMIT-ITEM
               PERFORM 3330-ASSESS-PREFERRED
               PERFORM 3340-COUNT-RETRY
               PERFORM 3350-AGE-EFF
               PERFORM 3360-AGE-LAST-STMT
               PERFORM 3370-PRORATE-AMOUNT-2
               PERFORM 3380-PRORATE-AMOUNT-2
               PERFORM 3390-COUNT-CYCLE
               PERFORM 3400-PRORATE-AMOUNT-2
               PERFORM 3410-SPLIT-AMOUNT-2
               PERFORM 3420-LIMIT-CONTROL
               PERFORM 3430-ACCUM-PIVOT
               PERFORM 3440-ACCUM-PIVOT
               PERFORM 3450-LIMIT-AMOUNT-2
               PERFORM 3460-PRORATE-CONTROL
               PERFORM 3470-COUNT-CYCLE
               PERFORM 3480-COUNT-PAYMENT
               PERFORM 3490-DESCRIBE-TRAN
               PERFORM 3500-EDIT-MEMO
               PERFORM 3510-FLAG-ITEM
               PERFORM 3520-SET-WINDOW-RULE
               PERFORM 3530-CHECK-LAST-MAINT
               PERFORM 3540-ACCUM-PIVOT
               PERFORM 3550-FLAG-AMOUNT-1
               PERFORM 3560-ASSESS-EXEMPT
               PERFORM 3570-DESCRIBE-SOURCE
               PERFORM 3580-ACCUM-TRAN
               PERFORM 3590-CHECK-EXPIRY
               PERFORM 3600-SELECT-MARGIN
               PERFORM 3610-LIMIT-AMOUNT-1
               PERFORM 3620-SET-PIVOT
               PERFORM 3630-SPLIT-AMOUNT-2
               PERFORM 3640-ASSESS-REVIEW
               PERFORM 3650-COUNT-RETRY
               PERFORM 3660-SELECT-MARGIN
               PERFORM 3670-ACCUM-REASON
               PERFORM 3680-ACCRUE-CONTROL
               PERFORM 3690-ACCRUE-CONTROL
               PERFORM 3700-ACCUM-CHANNEL
               PERFORM 3710-AGE-OPEN
               PERFORM 3720-EDIT-NAME
               PERFORM 3730-ASSESS-REVERSAL
               PERFORM 3740-EDIT-CONTACT
               PERFORM 3750-EDIT-OPERATOR
               PERFORM 3760-SPLIT-AMOUNT-2
               PERFORM 3770-FLAG-AMOUNT-1
               PERFORM 3780-ACCUM-REGION
               PERFORM 3790-LIMIT-AMOUNT-2
               PERFORM 3800-SET-SOURCE
               PERFORM 3810-SELECT-MARGIN
               PERFORM 3820-PRORATE-CONTROL
               PERFORM 3830-LIMIT-CONTROL
               PERFORM 3840-SET-REASON
               PERFORM 3850-FLAG-CONTROL
               PERFORM 3860-ASSESS-REVIEW
               PERFORM 3870-CHECK-EFF
               PERFORM 3880-DESCRIBE-TRAN
               PERFORM 3890-LIMIT-AMOUNT-1
               PERFORM 3900-COUNT-CYCLE
               PERFORM 3910-ACCUM-CYCLE
               PERFORM 3920-SET-CYCLE
               PERFORM 3930-EDIT-REF-NO
               PERFORM 3940-ACCRUE-CONTROL
               PERFORM 3950-ACCUM-REASON
               PERFORM 3960-SPLIT-AMOUNT-1
               PERFORM 3970-ACCRUE-DISCOUNT
               PERFORM 3980-CHECK-LAST-MAINT
               PERFORM 3990-DESCRIBE-CHANNEL
               PERFORM 4000-SPLIT-AMOUNT-1
               PERFORM 4010-DESCRIBE-CHANNEL
               PERFORM 4020-SET-REGION
               PERFORM 4030-AGE-EFF
               PERFORM 4040-DESCRIBE-SOURCE
               PERFORM 4050-AGE-NEXT-DUE
               PERFORM 4060-DESCRIBE-REGION
               PERFORM 4070-SELECT-PENALTY-PCT
               PERFORM 4080-EDIT-CONTACT
               PERFORM 4090-PRORATE-CONTROL
               PERFORM 4100-CHECK-LAST-MAINT
               PERFORM 4110-DESCRIBE-SOURCE
               PERFORM 4120-ACCRUE-AMOUNT-1
               PERFORM 4130-ASSESS-REVIEW
               PERFORM 4140-EDIT-ADDR
               PERFORM 4150-LIMIT-AMOUNT-1
               PERFORM 4160-EDIT-NAME
               PERFORM 4170-ACCUM-REASON
               PERFORM 4180-ASSESS-AUTO-RENEW
               PERFORM 4190-DESCRIBE-PIVOT
               PERFORM 4200-COUNT-CYCLE
               PERFORM 7000-CALL-YKPRC
               PERFORM 7010-CALL-YKTIER
               PERFORM 7030-CALL-YKWAIV
               REWRITE YDB-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'YDBMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM YDB-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN NEXT DUE DATE IS PAST
      * NNG -> PTC WHEN NEXT DUE DATE IS PAST
      * PTC -> WZW WHEN NEXT DUE DATE IS PAST
      * WZW -> KGT WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       3020-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-YTD ROUNDED =
                   YDBM-AMOUNT-1-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3030-FLAG-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-1-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3040-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 25.00
               MOVE 25.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 500.00
               MOVE 500.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBM-CONTROL-AMT NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-AMT
           END-IF
           ADD YDBM-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3120-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> LK WHEN NEXT DUE DATE IS PAST
      * LK -> NT WHEN NEXT DUE DATE IS PAST
      * NT -> UW WHEN NEXT DUE DATE IS PAST
      * UW -> AX WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NT' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NT'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UW' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UW'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AX' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3160-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN EXPIRY DATE IS PAST
      * FTB -> GAP WHEN EXPIRY DATE IS PAST
      * GAP -> PAP WHEN EXPIRY DATE IS PAST
      * PAP -> VGC WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-AMT ROUNDED =
                   YDBM-AMOUNT-2-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-AMT
           END-IF
           ADD YDBM-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 12
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD YDBM-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN POST DATE IS PAST
      * FTB -> GAP WHEN POST DATE IS PAST
      * GAP -> PAP WHEN POST DATE IS PAST
      * PAP -> VGC WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-2-AMT * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-1-AMT ROUNDED =
                   YDBM-CONTROL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 3
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBM-AMOUNT-2-YTD NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3270-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 12
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3280-FLAG-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CONTROL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3290-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = YDBM-CONTROL-AMT *
                       YDBM-DISCOUNT / 200
               WHEN YDBT-TRAN-CD = 'CV'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN YDBT-TRAN-CD = 'RV'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = YDBM-CONTROL-AMT *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-AMT - (WS-WORK-AMT * 2)
           COMPUTE YDBT-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3330-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = WS-WORK-AMT *
                       YDBM-DISCOUNT / 200
               WHEN YDBT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO YDBM-CONTROL-YTD
               WHEN YDBT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO YDBM-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = WS-WORK-AMT *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-CONTROL-YTD > 0 AND YDBM-CONTROL-YTD < 2.00
               MOVE 2.00 TO YDBM-CONTROL-YTD
           END-IF
           IF YDBM-CONTROL-YTD > 35.00
               MOVE 35.00 TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 3
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 YTD.
      *----------------------------------------------------------------*
       3370-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-YTD ROUNDED =
                   YDBM-CONTROL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-YTD
           END-IF
           ADD YDBM-AMOUNT-2-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-2-AMT ROUNDED =
                   YDBM-AMOUNT-1-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-2-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-YTD / 2
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-YTD - (WS-WORK-AMT * 2)
           COMPUTE YDBM-AMOUNT-1-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3420-LIMIT-CONTROL.
           IF YDBM-CONTROL-AMT < 25.00
               MOVE 25.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-AMT > 500.00
               MOVE 500.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL AMT BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-PIVOT-CD
                   ADD YDBT-CONTROL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-CONTROL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3440-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-PIVOT-CD
                   ADD YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3450-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT < 100.00
               MOVE 100.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 1000.00
               MOVE 1000.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTROL YTD.
      *----------------------------------------------------------------*
       3460-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-CONTROL-YTD ROUNDED =
                   YDBM-CONTROL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E497 MEMO REQUIRED
      * E100 AMOUNT 1 YTD NEGATIVE
      * E184 AMOUNT 1 YTD OVER 1000000
      * E587 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E497' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD < 0
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD > 1000000.00
                   MOVE 'E184' TO WS-REJECT-CODE
               WHEN YDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3510-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR WINDOW RULE CD:
      * A -> D WHEN LAST STMT DATE IS PAST
      * D -> S WHEN LAST STMT DATE IS PAST
      * S -> T WHEN LAST STMT DATE IS PAST
      * T -> A WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-PIVOT-CD
                   ADD YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3550-FLAG-AMOUNT-1.
           IF YDBM-AMOUNT-1-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 1 YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3560-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO YDBT-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-1-YTD
                       * YDBM-PENALTY-PCT / 200
               WHEN YDBT-TRAN-CD = 'CV'
                   MOVE 15.00 TO YDBT-CONTROL-AMT
               WHEN YDBT-TRAN-CD = 'RV'
                   MOVE 5.00 TO YDBT-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-1-YTD
                       * YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBT-CONTROL-AMT > 0 AND YDBT-CONTROL-AMT < 5.00
               MOVE 5.00 TO YDBT-CONTROL-AMT
           END-IF
           IF YDBT-CONTROL-AMT > 50.00
               MOVE 50.00 TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-TRAN-CD
                   ADD YDBM-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBM-AMOUNT-1-AMT NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-AMOUNT-1.
           IF YDBM-AMOUNT-1-AMT < 100.00
               MOVE 100.00 TO YDBM-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-1-AMT > 10000.00
               MOVE 10000.00 TO YDBM-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN LAST STMT DATE IS PAST
      * NNG -> PTC WHEN LAST STMT DATE IS PAST
      * PTC -> WZW WHEN LAST STMT DATE IS PAST
      * WZW -> KGT WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-2-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-2-AMT - (WS-WORK-AMT * 2)
           COMPUTE YDBM-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3640-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 200
               WHEN YDBM-REASON-CD = 'AX'
                   MOVE 3.00 TO YDBT-AMOUNT-2-AMT
               WHEN YDBM-REASON-CD = 'LK'
                   MOVE 7.00 TO YDBT-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBT-AMOUNT-2-AMT > 0 AND YDBT-AMOUNT-2-AMT < 2.00
               MOVE 2.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 50.00
               MOVE 50.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 3
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 GRID TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-GRID-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF YDBM-AMOUNT-2-AMT NOT < WS-AMOUNT-1-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-GRID-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REASON-CD
                   ADD YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-CONTROL.
           IF YDBM-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-YTD * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-CONTROL.
           IF YDBM-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-YTD * YDBM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO YDBT-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YDBM-OPEN-DATE > 0 AND YDBM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E698 NAME REQUIRED
      * E590 AMOUNT 1 AMT NEGATIVE
      * E372 AMOUNT 1 AMT OVER 50000
      * E514 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-NAME = SPACES
                   MOVE 'E698' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT < 0
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT > 50000.00
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN YDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE CONTROL AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3730-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBM-CONTROL-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-REASON-CD = 'AX'
                   MOVE 7.00 TO YDBM-AMOUNT-1-YTD
               WHEN YDBM-REASON-CD = 'LK'
                   MOVE 15.00 TO YDBM-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBM-CONTROL-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-YTD > 0 AND YDBM-AMOUNT-1-YTD < 10.00
               MOVE 10.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           IF YDBM-AMOUNT-1-YTD > 75.00
               MOVE 75.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E188 CONTACT REQUIRED
      * E840 CONTROL AMT NEGATIVE
      * E413 CONTROL AMT OVER 250000
      * E166 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT < 0
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT > 250000.00
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN YDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E023 OPERATOR REQUIRED
      * E623 CONTROL AMT NEGATIVE
      * E437 CONTROL AMT OVER 1000000
      * E856 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-OPERATOR = SPACES
                   MOVE 'E023' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT < 0
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT > 1000000.00
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN YDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-2-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-2-AMT - (WS-WORK-AMT * 2)
           COMPUTE YDBM-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3770-FLAG-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-1-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD YDBM-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3790-LIMIT-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT < 0.00
               MOVE 0.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 500.00
               MOVE 500.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN EXPIRY DATE IS PAST
      * FTB -> GAP WHEN EXPIRY DATE IS PAST
      * GAP -> PAP WHEN EXPIRY DATE IS PAST
      * PAP -> VGC WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-GRID-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-AMOUNT-1-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-GRID-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3820-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   YDBM-CONTROL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3830-LIMIT-CONTROL.
           IF YDBT-CONTROL-AMT < 50.00
               MOVE 50.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-CONTROL-AMT > 500.00
               MOVE 500.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> LK WHEN POST DATE IS PAST
      * LK -> NT WHEN POST DATE IS PAST
      * NT -> UW WHEN POST DATE IS PAST
      * UW -> AX WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NT' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NT'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UW' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UW'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AX' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3850-FLAG-CONTROL.
           IF YDBM-CONTROL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3860-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-WINDOW-RULE-CD = 'A'
                   MOVE 15.00 TO YDBT-AMOUNT-2-AMT
               WHEN YDBM-WINDOW-RULE-CD = 'D'
                   MOVE 15.00 TO YDBT-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-AMOUNT-2-AMT > 0 AND YDBT-AMOUNT-2-AMT < 5.00
               MOVE 5.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 250.00
               MOVE 250.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3890-LIMIT-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT < 0.00
               MOVE 0.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 2500.00
               MOVE 2500.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN POST DATE IS PAST
      * LA -> SP WHEN POST DATE IS PAST
      * SP -> UC WHEN POST DATE IS PAST
      * UC -> FJ WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LA' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E127 REF NO REQUIRED
      * E908 CONTROL YTD NEGATIVE
      * E151 CONTROL YTD OVER 25000
      * E508 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-REF-NO = SPACES
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD < 0
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD > 25000.00
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E508' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-CONTROL.
           IF YDBM-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-AMT * YDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REASON-CD
                   ADD YDBM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-1-AMT / 12
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-1-AMT - (WS-WORK-AMT *
               12)
           COMPUTE YDBM-AMOUNT-2-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YDBT-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-1-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-1-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN EXPIRY DATE IS PAST
      * H -> K WHEN EXPIRY DATE IS PAST
      * K -> S WHEN EXPIRY DATE IS PAST
      * S -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-REGION.
           EVALUATE YDBM-REGION-CD
               WHEN 'C'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-REGION.
           EVALUATE YDBM-REGION-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-GRID-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-AMOUNT-1-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-GRID-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E325 CONTACT REQUIRED
      * E179 AMOUNT 1 AMT NEGATIVE
      * E916 AMOUNT 1 AMT OVER 1000000
      * E190 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-AMT < 0
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-AMT > 1000000.00
                   MOVE 'E916' TO WS-REJECT-CODE
               WHEN YDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       4090-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-CONTROL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-1-YTD * YDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4130-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = YDBT-AMOUNT-1-AMT *
                       YDBM-PENALTY-PCT / 200
               WHEN YDBM-WINDOW-RULE-CD = 'A'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN YDBM-WINDOW-RULE-CD = 'D'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = YDBT-AMOUNT-1-AMT *
                       YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E744 ADDR REQUIRED
      * E122 AMOUNT 1 AMT NEGATIVE
      * E745 AMOUNT 1 AMT OVER 10000
      * E054 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-ADDR = SPACES
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT < 0
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT > 10000.00
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN YDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4150-LIMIT-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT < 0.00
               MOVE 0.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 500.00
               MOVE 500.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E794 NAME REQUIRED
      * E759 AMOUNT 1 YTD NEGATIVE
      * E706 AMOUNT 1 YTD OVER 5000
      * E926 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-NAME = SPACES
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD < 0
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD > 5000.00
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN YDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REASON-CD
                   ADD YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4180-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = WS-WORK-AMT *
                       YDBM-DISCOUNT / 200
               WHEN YDBM-CYCLE-CD = 'FJ'
                   MOVE 15.00 TO YDBM-CONTROL-AMT
               WHEN YDBM-CYCLE-CD = 'LA'
                   MOVE 12.00 TO YDBM-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = WS-WORK-AMT *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-CONTROL-AMT > 0 AND YDBM-CONTROL-AMT < 1.00
               MOVE 1.00 TO YDBM-CONTROL-AMT
           END-IF
           IF YDBM-CONTROL-AMT > 35.00
               MOVE 35.00 TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-YKPRC.
           INITIALIZE YKPRC-PARMS
           MOVE YDBM-AMOUNT-1-AMT
             TO YKPRC-AMOUNT-1-AMT
           MOVE YDBM-CONTROL-YTD
             TO YKPRC-CONTROL-YTD
           MOVE YDBM-AMOUNT-1-YTD
             TO YKPRC-AMOUNT-1-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKPRC-AMOUNT-2-AMT
           MOVE YDBM-CONTROL-AMT
             TO YKPRC-CONTROL-AMT
           MOVE YDBM-AMOUNT-2-YTD
             TO YKPRC-AMOUNT-2-YTD
           MOVE YDBM-EXPIRY-DATE
             TO YKPRC-EXPIRY-DATE
           MOVE YDBM-LAST-STMT-DATE
             TO YKPRC-LAST-STMT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKPRC-MATURITY-DATE
           MOVE YDBM-LAST-ACT-DATE
             TO YKPRC-LAST-ACT-DATE
           MOVE YDBM-CYCLE-CD
             TO YKPRC-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKPRC-SOURCE-CD
           MOVE YDBM-PIVOT-CD
             TO YKPRC-PIVOT-CD
           MOVE YDBM-REGION-CD
             TO YKPRC-REGION-CD
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKPRC-NOTICE-SENT-FLAG
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKPRC-AUTO-RENEW-FLAG
           MOVE YDBM-PREFERRED-FLAG
             TO YKPRC-PREFERRED-FLAG
           MOVE YDBM-ITEM-CNT
             TO YKPRC-ITEM-CNT
           MOVE YDBM-PAYMENT-CNT
             TO YKPRC-PAYMENT-CNT
           MOVE YDBM-MARGIN
             TO YKPRC-MARGIN
           MOVE YDBM-DISCOUNT
             TO YKPRC-DISCOUNT
           MOVE YDBM-NAME
             TO YKPRC-NAME
           MOVE YDBM-CONTACT
             TO YKPRC-CONTACT
           CALL 'YKPRC' USING YKPRC-PARMS
           EVALUATE YKPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKPRC-AMOUNT-1-AMT
                     TO YDBM-AMOUNT-1-AMT
                   MOVE YKPRC-NOTICE-SENT-FLAG
                     TO YDBM-NOTICE-SENT-FLAG
                   MOVE YKPRC-AUTO-RENEW-FLAG
                     TO YDBM-AUTO-RENEW-FLAG
                   MOVE YKPRC-PREFERRED-FLAG
                     TO YDBM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-YKPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-YKTIER.
           INITIALIZE YKTIER-PARMS
           MOVE YDBM-AMOUNT-2-YTD
             TO YKTIER-AMOUNT-2-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKTIER-CONTROL-YTD
           MOVE YDBM-AMOUNT-1-YTD
             TO YKTIER-AMOUNT-1-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKTIER-AMOUNT-2-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKTIER-AMOUNT-1-AMT
           MOVE YDBM-CONTROL-AMT
             TO YKTIER-CONTROL-AMT
           MOVE YDBM-LAST-ACT-DATE
             TO YKTIER-LAST-ACT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKTIER-MATURITY-DATE
           MOVE YDBM-LAST-MAINT-DATE
             TO YKTIER-LAST-MAINT-DATE
           MOVE YDBM-EXPIRY-DATE
             TO YKTIER-EXPIRY-DATE
           MOVE YDBM-CYCLE-CD
             TO YKTIER-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKTIER-SOURCE-CD
           MOVE YDBM-REGION-CD
             TO YKTIER-REGION-CD
           MOVE YDBM-PIVOT-CD
             TO YKTIER-PIVOT-CD
           MOVE YDBM-PREFERRED-FLAG
             TO YKTIER-PREFERRED-FLAG
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKTIER-AUTO-RENEW-FLAG
           MOVE YDBM-REVIEW-FLAG
             TO YKTIER-REVIEW-FLAG
           MOVE YDBM-ITEM-CNT
             TO YKTIER-ITEM-CNT
           MOVE YDBM-RETRY-CNT
             TO YKTIER-RETRY-CNT
           MOVE YDBM-DISCOUNT
             TO YKTIER-DISCOUNT
           MOVE YDBM-PENALTY-PCT
             TO YKTIER-PENALTY-PCT
           MOVE YDBM-CUST-ID
             TO YKTIER-CUST-ID
           MOVE YDBM-CONTACT
             TO YKTIER-CONTACT
           CALL 'YKTIER' USING YKTIER-PARMS
           EVALUATE YKTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKTIER-CONTROL-YTD
                     TO YDBM-CONTROL-YTD
                   MOVE YKTIER-AMOUNT-2-AMT
                     TO YDBM-AMOUNT-2-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-YKTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-YKWAIV.
           INITIALIZE YKWAIV-PARMS
           MOVE YDBM-AMOUNT-1-YTD
             TO YKWAIV-AMOUNT-1-YTD
           MOVE YDBM-CONTROL-AMT
             TO YKWAIV-CONTROL-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKWAIV-AMOUNT-1-AMT
           MOVE YDBM-AMOUNT-2-YTD
             TO YKWAIV-AMOUNT-2-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKWAIV-CONTROL-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKWAIV-AMOUNT-2-AMT
           MOVE YDBM-NOTICE-DATE
             TO YKWAIV-NOTICE-DATE
           MOVE YDBM-LAST-ACT-DATE
             TO YKWAIV-LAST-ACT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKWAIV-MATURITY-DATE
           MOVE YDBM-NEXT-DUE-DATE
             TO YKWAIV-NEXT-DUE-DATE
           MOVE YDBM-SOURCE-CD
             TO YKWAIV-SOURCE-CD
           MOVE YDBM-CYCLE-CD
             TO YKWAIV-CYCLE-CD
           MOVE YDBM-REASON-CD
             TO YKWAIV-REASON-CD
           MOVE YDBM-REGION-CD
             TO YKWAIV-REGION-CD
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKWAIV-AUTO-RENEW-FLAG
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKWAIV-NOTICE-SENT-FLAG
           MOVE YDBM-PREFERRED-FLAG
             TO YKWAIV-PREFERRED-FLAG
           MOVE YDBM-PAYMENT-CNT
             TO YKWAIV-PAYMENT-CNT
           MOVE YDBM-CYCLE-CNT
             TO YKWAIV-CYCLE-CNT
           MOVE YDBM-DISCOUNT
             TO YKWAIV-DISCOUNT
           MOVE YDBM-MARGIN
             TO YKWAIV-MARGIN
           MOVE YDBM-CUST-ID
             TO YKWAIV-CUST-ID
           MOVE YDBM-CONTACT
             TO YKWAIV-CONTACT
           CALL 'YKWAIV' USING YKWAIV-PARMS
           EVALUATE YKWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKWAIV-AMOUNT-1-YTD
                     TO YDBM-AMOUNT-1-YTD
                   MOVE YKWAIV-CONTROL-AMT
                     TO YDBM-CONTROL-AMT
                   MOVE YKWAIV-AMOUNT-1-AMT
                     TO YDBM-AMOUNT-1-AMT
                   MOVE YKWAIV-AMOUNT-2-YTD
                     TO YDBM-AMOUNT-2-YTD
                   MOVE YKWAIV-CONTROL-YTD
                     TO YDBM-CONTROL-YTD
                   MOVE YKWAIV-AUTO-RENEW-FLAG
                     TO YDBM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-YKWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE YDBT-KEY TO RD-KEY
           MOVE YDBT-TRAN-CD TO RD-CODE
           MOVE YDBT-AMOUNT-1-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE YDBT-KEY TO RD-KEY
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
           DISPLAY 'YKBCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'YKBCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
