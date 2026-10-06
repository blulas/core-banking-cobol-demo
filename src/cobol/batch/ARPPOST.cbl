      *================================================================*
      * PROGRAM  : ARPPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ACCOUNT RECONCILEMENT - DAILY POSTING. READS THE ARP*
      *            TRANSACTIONS, UPDATES THE ARP MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FULL AND PARTIAL ACCOUNT        *
      *            RECONCILEMENT SERVICE FOR CORPORATES                *
      * INPUT    : ARPMST (KSDS), ARPTRN, BANKCTL                      *
      * OUTPUT   : ARPOUT, ARPPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ARRISK, ARDTE, ARPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-15 MKL  ORIGINAL                                       *
      * 1996-06-27 EWS  OUTSTAND EXCEPTIONS TO OPS QUEUE REQ5977       *
      * 2001-08-06 GAF  FEE CALCULATION CORRECTED CHG4194              *
      * 2001-10-27 BHS  PERFORMANCE - OUTSTAND READ AHEAD PRB8969      *
      * 2001-09-11 EWS  OUTSTAND STATUS CODES ADDED REQ7559            *
      * 2002-03-11 MKL  OUTSTAND CALCULATION CORRECTED INC1012         *
      * 2006-04-11 TWH  ISSUED LIMIT REVISED CHG2750                   *
      * 2012-01-19 SKN  PERFORMANCE - FEE READ AHEAD REQ1464           *
      * 2013-07-24 EWS  OUTSTAND TABLE EXPANDED CHG0882                *
      * 2013-12-19 SKN  PAID TABLE EXPANDED INC4976                    *
      * 2014-12-20 JLM  PAID NOTICE TEXT PER LEGAL INC4804             *
      * 2014-05-18 BHS  PAID TABLE EXPANDED INC8564                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPPOST.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ARPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ARPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ARPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ARPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ARPPORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ARPMST.
       FD  TRAN-FILE.
       COPY ARPTRN.
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
                                           VALUE 15.000000.
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
      * WS-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000843000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000322000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000865000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000511000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000243500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000467000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000179500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019500000000450000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000473000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030500000000746500'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 10 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000714000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000466000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000688500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000358500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000187000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008850000000286000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009850000000438500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009950000000805500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000815000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000518500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000540500'.
       01  WS-PAID-SCHED-TBL REDEFINES WS-PAID-SCHED-VALUES.
           05  WS-PAID-SCHED-ROW OCCURS 11 TIMES.
               10  WS-PAID-SCHED-LOW       PIC 9(09)V99.
               10  WS-PAID-SCHED-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ARPPOST  ACCOUNT RECONCILEMENT'.
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
       COPY ARRISKP.
       COPY ARDTEP.
       COPY ARPRCP.

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
               MOVE 'ARPMST' TO ABND-FILE-NAME
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
           MOVE ARPT-KEY TO ARPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-SOURCE
               PERFORM 3010-SET-SOURCE
               PERFORM 3020-SELECT-PENALTY-PCT
               PERFORM 3030-SELECT-PENALTY-PCT
               PERFORM 3040-COUNT-ITEM
               PERFORM 3050-ACCRUE-FEE
               PERFORM 3060-ASSESS-PREFERRED
               PERFORM 3070-LIMIT-OUTSTAND
               PERFORM 3080-SET-CHANNEL
               PERFORM 3090-COUNT-CYCLE
               PERFORM 3100-ACCUM-CHANNEL
               PERFORM 3110-ACCUM-BRANCH
               PERFORM 3120-SELECT-DISCOUNT
               PERFORM 3130-ACCUM-CHANNEL
               PERFORM 3140-SET-REGION
               PERFORM 3150-DESCRIBE-REGION
               PERFORM 3160-PRORATE-ISSUED
               PERFORM 3170-LIMIT-ITEM
               PERFORM 3180-ACCUM-TRAN
               PERFORM 3190-SELECT-FEE-PCT
               PERFORM 3200-SELECT-FEE-PCT
               PERFORM 3210-EDIT-CUST-ID
               PERFORM 3220-SELECT-PENALTY-PCT
               PERFORM 3230-ASSESS-PREFERRED
               PERFORM 3240-ACCUM-SOURCE
               PERFORM 3250-LIMIT-VOID
               PERFORM 3260-COUNT-PAYMENT
               PERFORM 3270-SET-BRANCH
               PERFORM 3280-EDIT-CUST-ID
               PERFORM 3290-DESCRIBE-CHANNEL
               PERFORM 3300-ASSESS-REVERSAL
               PERFORM 3310-PRORATE-FEE
               PERFORM 3320-AGE-LAST-PAY
               PERFORM 3330-ACCRUE-ISSUED
               PERFORM 3340-SET-CHANNEL
               PERFORM 3350-COUNT-EXCEPT
               PERFORM 3360-SPLIT-VOID
               PERFORM 3370-PRORATE-VOID
               PERFORM 3380-SET-CHANNEL
               PERFORM 3390-ACCRUE-OUTSTAND
               PERFORM 3400-SET-BRANCH
               PERFORM 3410-ACCRUE-VOID
               PERFORM 3420-CHECK-EFFECTIVE
               PERFORM 3430-ASSESS-EXEMPT
               PERFORM 3440-SPLIT-FEE
               PERFORM 3450-CHECK-LAST-ACT
               PERFORM 3460-FLAG-ITEM
               PERFORM 3470-ACCUM-REGION
               PERFORM 3480-PRORATE-OUTSTAND
               PERFORM 3490-SPLIT-PAID
               PERFORM 3500-SET-CYCLE
               PERFORM 3510-DESCRIBE-CHANNEL
               PERFORM 3520-SPLIT-OUTSTAND
               PERFORM 3530-CHECK-LAST-PAY
               PERFORM 3540-SET-CHANNEL
               PERFORM 3550-SET-CYCLE
               PERFORM 3560-FLAG-OUTSTAND
               PERFORM 3570-FLAG-PAID
               PERFORM 3580-CHECK-NEXT-REVIEW
               PERFORM 3590-FLAG-ISSUED
               PERFORM 3600-ASSESS-NOTICE-SENT
               PERFORM 3610-SELECT-DISCOUNT
               PERFORM 3620-ASSESS-REVERSAL
               PERFORM 3630-ACCUM-REGION
               PERFORM 3640-DESCRIBE-CHANNEL
               PERFORM 3650-CHECK-LAST-STMT
               PERFORM 3660-ACCRUE-ISSUED
               PERFORM 3670-DESCRIBE-BRANCH
               PERFORM 3680-SELECT-PENALTY-PCT
               PERFORM 3690-COUNT-EXCEPT
               PERFORM 3700-LIMIT-ISSUED
               PERFORM 3710-EDIT-DESC
               PERFORM 3720-SPLIT-OUTSTAND
               PERFORM 3730-EDIT-DESC
               PERFORM 3740-DESCRIBE-RECON-TYPE
               PERFORM 3750-SET-TRAN
               PERFORM 3760-EDIT-ADDR
               PERFORM 3770-EDIT-ADDR
               PERFORM 3780-EDIT-ADDR
               PERFORM 3790-AGE-NEXT-DUE
               PERFORM 3800-SELECT-FEE-PCT
               PERFORM 3810-ASSESS-REVERSAL
               PERFORM 3820-DESCRIBE-BRANCH
               PERFORM 3830-SELECT-FEE-PCT
               PERFORM 3840-DESCRIBE-SOURCE
               PERFORM 3850-ACCUM-CHANNEL
               PERFORM 3860-PRORATE-OUTSTAND
               PERFORM 3870-FLAG-STOP
               PERFORM 3880-LIMIT-ISSUED
               PERFORM 3890-ACCUM-SOURCE
               PERFORM 3900-FLAG-VOID
               PERFORM 3910-ACCRUE-PAID
               PERFORM 3920-LIMIT-FEE
               PERFORM 3930-COUNT-PAYMENT
               PERFORM 3940-SELECT-PENALTY-PCT
               PERFORM 3950-SPLIT-VOID
               PERFORM 3960-PRORATE-PAID
               PERFORM 3970-ASSESS-REVERSAL
               PERFORM 3980-ASSESS-NOTICE-SENT
               PERFORM 3990-CHECK-NEXT-REVIEW
               PERFORM 4000-AGE-NEXT-DUE
               PERFORM 4010-CHECK-OPEN
               PERFORM 4020-FLAG-OUTSTAND
               PERFORM 4030-SET-CHANNEL
               PERFORM 4040-AGE-POST
               PERFORM 4050-ACCUM-TRAN
               PERFORM 4060-ACCRUE-ISSUED
               PERFORM 4070-DESCRIBE-BRANCH
               PERFORM 4080-FLAG-OUTSTAND
               PERFORM 4090-COUNT-EXCEPT
               PERFORM 4100-SET-CHANNEL
               PERFORM 4110-AGE-EFF
               PERFORM 4120-SET-RECON-TYPE
               PERFORM 4130-FLAG-OUTSTAND
               PERFORM 4140-SELECT-FEE-PCT
               PERFORM 4150-ACCRUE-FEE
               PERFORM 4160-SPLIT-OUTSTAND
               PERFORM 4170-CHECK-CLOSE
               PERFORM 4180-ASSESS-STOP
               PERFORM 4190-EDIT-ADDR
               PERFORM 4200-DESCRIBE-TRAN
               PERFORM 4210-LIMIT-STOP
               PERFORM 4220-FLAG-ISSUED
               PERFORM 4230-CHECK-LAST-STMT
               PERFORM 4240-COUNT-ITEM
               PERFORM 4250-LIMIT-ITEM
               PERFORM 4260-FLAG-ITEM
               PERFORM 4270-SELECT-PENALTY-PCT
               PERFORM 4280-ASSESS-REVERSAL
               PERFORM 4290-SPLIT-OUTSTAND
               PERFORM 4300-ACCUM-CYCLE
               PERFORM 4310-PRORATE-STOP
               PERFORM 4320-LIMIT-VOID
               PERFORM 4330-SPLIT-FEE
               PERFORM 4340-COUNT-PAYMENT
               PERFORM 4350-ASSESS-STOP
               PERFORM 7030-CALL-ARRISK
               PERFORM 7020-CALL-ARDTE
               PERFORM 7000-CALL-ARPRC
               REWRITE ARP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ARPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ARP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN LAST MAINT DATE IS PAST
      * MB -> MH WHEN LAST MAINT DATE IS PAST
      * MH -> UG WHEN LAST MAINT DATE IS PAST
      * UG -> UR WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-ISSUED-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPT-ISSUED-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-FEE.
           IF ARPM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-FEE-YTD * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-STOP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID YTD FEE ON THE VOID AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3060-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-STOP-FLAG = 'Y'
                   COMPUTE ARPM-VOID-YTD ROUNDED = ARPT-VOID-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CHANNEL-CD = 'ME'
                   MOVE 20.00 TO ARPM-VOID-YTD
               WHEN ARPM-CHANNEL-CD = 'SW'
                   MOVE 10.00 TO ARPM-VOID-YTD
               WHEN OTHER
                   COMPUTE ARPM-VOID-YTD ROUNDED = ARPT-VOID-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-VOID-YTD > 0 AND ARPM-VOID-YTD < 2.00
               MOVE 2.00 TO ARPM-VOID-YTD
           END-IF
           IF ARPM-VOID-YTD > 100.00
               MOVE 100.00 TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3070-LIMIT-OUTSTAND.
           IF ARPM-OUTSTAND-AMT < 25.00
               MOVE 25.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-OUTSTAND-AMT > 2500.00
               MOVE 2500.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN LAST STMT DATE IS PAST
      * SW -> UA WHEN LAST STMT DATE IS PAST
      * UA -> WB WHEN LAST STMT DATE IS PAST
      * WB -> ME WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD ARPM-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-BRANCH-CD
                   ADD ARPM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-ISSUED-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-CHANNEL-CD
                   ADD ARPM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN EFFECTIVE DATE IS PAST
      * BU -> GM WHEN EFFECTIVE DATE IS PAST
      * GM -> SW WHEN EFFECTIVE DATE IS PAST
      * SW -> WD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3160-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD ARPM-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-ISSUED-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-ISSUED-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E894 CUST ID REQUIRED
      * E466 OUTSTAND YTD NEGATIVE
      * E218 OUTSTAND YTD OVER 50000
      * E465 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E466' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 50000.00
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-FEE-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STOP YTD FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3230-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-STOP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE ARPM-STOP-YTD ROUNDED = ARPM-OUTSTAND-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-REGION-CD = 'AD'
                   MOVE 10.00 TO ARPM-STOP-YTD
               WHEN ARPM-REGION-CD = 'BU'
                   MOVE 12.00 TO ARPM-STOP-YTD
               WHEN OTHER
                   COMPUTE ARPM-STOP-YTD ROUNDED = ARPM-OUTSTAND-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-STOP-YTD > 0 AND ARPM-STOP-YTD < 10.00
               MOVE 10.00 TO ARPM-STOP-YTD
           END-IF
           IF ARPM-STOP-YTD > 50.00
               MOVE 50.00 TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-SOURCE-CD
                   ADD ARPM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-OUTSTAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-VOID.
           IF ARPM-VOID-AMT < 100.00
               MOVE 100.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-VOID-AMT > 50000.00
               MOVE 50000.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN NEXT DUE DATE IS PAST
      * GM -> GV WHEN NEXT DUE DATE IS PAST
      * GV -> HK WHEN NEXT DUE DATE IS PAST
      * HK -> VX WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E172 CUST ID REQUIRED
      * E058 WORK AMT 2 NEGATIVE
      * E141 WORK AMT 2 OVER 1000000
      * E141 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E141' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E141' TO WS-REJECT-CODE
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
       3290-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE VOID AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3300-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPM-ISSUED-AMT ROUNDED = ARPT-VOID-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO ARPM-ISSUED-AMT
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO ARPM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPM-ISSUED-AMT ROUNDED = ARPT-VOID-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-ISSUED-AMT > 0 AND ARPM-ISSUED-AMT < 1.00
               MOVE 1.00 TO ARPM-ISSUED-AMT
           END-IF
           IF ARPM-ISSUED-AMT > 50.00
               MOVE 50.00 TO ARPM-ISSUED-AMT
           END-IF
           ADD ARPM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-ISSUED.
           IF ARPM-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-YTD * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPT-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN OPEN DATE IS PAST
      * SW -> UA WHEN OPEN DATE IS PAST
      * UA -> WB WHEN OPEN DATE IS PAST
      * WB -> ME WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ARPM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPM-VOID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-VOID-AMT - (WS-WORK-AMT * 2)
           COMPUTE ARPM-STOP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3370-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-VOID-AMT ROUNDED =
                   ARPM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-VOID-AMT
           END-IF
           ADD ARPT-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN LAST ACT DATE IS PAST
      * SW -> UA WHEN LAST ACT DATE IS PAST
      * UA -> WB WHEN LAST ACT DATE IS PAST
      * WB -> ME WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN LAST STMT DATE IS PAST
      * GM -> GV WHEN LAST STMT DATE IS PAST
      * GV -> HK WHEN LAST STMT DATE IS PAST
      * HK -> VX WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-VOID.
           IF ARPM-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-AMT * ARPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE STOP AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3430-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 20.00 TO ARPM-FEE-AMT
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 3.00 TO ARPM-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-FEE-AMT > 0 AND ARPM-FEE-AMT < 5.00
               MOVE 5.00 TO ARPM-FEE-AMT
           END-IF
           IF ARPM-FEE-AMT > 25.00
               MOVE 25.00 TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPT-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPT-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3460-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-REGION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3480-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   ARPM-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARPM-PAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-PAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CP -> MR WHEN LAST STMT DATE IS PAST
      * MR -> PY WHEN LAST STMT DATE IS PAST
      * PY -> SP WHEN LAST STMT DATE IS PAST
      * SP -> TP WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN NEXT DUE DATE IS PAST
      * SW -> UA WHEN NEXT DUE DATE IS PAST
      * UA -> WB WHEN NEXT DUE DATE IS PAST
      * WB -> ME WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CP -> MR WHEN LAST STMT DATE IS PAST
      * MR -> PY WHEN LAST STMT DATE IS PAST
      * PY -> SP WHEN LAST STMT DATE IS PAST
      * SP -> TP WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3560-FLAG-OUTSTAND.
           IF ARPT-OUTSTAND-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPT-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3570-FLAG-PAID.
           IF ARPM-PAID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3590-FLAG-ISSUED.
           IF ARPM-ISSUED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-ISSUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STOP FEE ON THE PAID YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3600-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-STOP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPM-STOP-AMT ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-SOURCE-CD = 'GW'
                   MOVE 10.00 TO ARPM-STOP-AMT
               WHEN ARPM-SOURCE-CD = 'MB'
                   MOVE 15.00 TO ARPM-STOP-AMT
               WHEN OTHER
                   COMPUTE ARPM-STOP-AMT ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-STOP-AMT > 0 AND ARPM-STOP-AMT < 10.00
               MOVE 10.00 TO ARPM-STOP-AMT
           END-IF
           IF ARPM-STOP-AMT > 250.00
               MOVE 250.00 TO ARPM-STOP-AMT
           END-IF
           ADD ARPM-STOP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3620-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-STOP-FLAG = 'Y'
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPT-FEE-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CHANNEL-CD = 'ME'
                   MOVE 15.00 TO ARPM-FEE-AMT
               WHEN ARPM-CHANNEL-CD = 'SW'
                   MOVE 10.00 TO ARPM-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPT-FEE-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-AMT > 0 AND ARPM-FEE-AMT < 1.00
               MOVE 1.00 TO ARPM-FEE-AMT
           END-IF
           IF ARPM-FEE-AMT > 100.00
               MOVE 100.00 TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-REGION-CD
                   ADD ARPM-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-ISSUED.
           IF ARPM-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-YTD * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPM-FEE-YTD NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-ISSUED.
           IF ARPM-ISSUED-AMT < 50.00
               MOVE 50.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-AMT > 5000.00
               MOVE 5000.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E251 DESC REQUIRED
      * E358 FEE YTD NEGATIVE
      * E120 FEE YTD OVER 250000
      * E511 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN ARPM-FEE-YTD < 0
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN ARPM-FEE-YTD > 250000.00
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN ARPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ARPM-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E107 DESC REQUIRED
      * E640 FEE AMT NEGATIVE
      * E453 FEE AMT OVER 250000
      * E988 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT < 0
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT > 250000.00
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3740-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST PAY DATE IS PAST
      * PD -> SP WHEN LAST PAY DATE IS PAST
      * SP -> VD WHEN LAST PAY DATE IS PAST
      * VD -> FE WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E520 ADDR REQUIRED
      * E665 PAID AMT NEGATIVE
      * E951 PAID AMT OVER 5000
      * E293 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT < 0
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT > 5000.00
                   MOVE 'E951' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E377 ADDR REQUIRED
      * E285 PAID AMT NEGATIVE
      * E822 PAID AMT OVER 10000
      * E689 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT < 0
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT > 10000.00
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E366 ADDR REQUIRED
      * E681 OUTSTAND YTD NEGATIVE
      * E386 OUTSTAND YTD OVER 25000
      * E293 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 25000.00
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE STOP AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3810-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-VOID-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CHANNEL-CD = 'ME'
                   MOVE 20.00 TO ARPM-VOID-AMT
               WHEN ARPM-CHANNEL-CD = 'SW'
                   MOVE 5.00 TO ARPM-VOID-AMT
               WHEN OTHER
                   COMPUTE ARPM-VOID-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-VOID-AMT > 0 AND ARPM-VOID-AMT < 5.00
               MOVE 5.00 TO ARPM-VOID-AMT
           END-IF
           IF ARPM-VOID-AMT > 75.00
               MOVE 75.00 TO ARPM-VOID-AMT
           END-IF
           ADD ARPM-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPT-OUTSTAND-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-OUTSTAND-AMT ROUNDED =
                   ARPT-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-OUTSTAND-AMT
           END-IF
           ADD ARPT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STOP AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-STOP.
           IF ARPM-STOP-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-STOP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-ISSUED.
           IF ARPM-ISSUED-AMT < 50.00
               MOVE 50.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-AMT > 500.00
               MOVE 500.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-SOURCE-CD
                   ADD ARPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3900-FLAG-VOID.
           IF ARPT-VOID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPT-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-PAID.
           IF ARPM-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-PAID-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3920-LIMIT-FEE.
           IF ARPM-FEE-AMT < 100.00
               MOVE 100.00 TO ARPM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-FEE-AMT > 500.00
               MOVE 500.00 TO ARPM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPT-OUTSTAND-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPM-VOID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-VOID-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STOP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3960-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-YTD ROUNDED =
                   ARPM-STOP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-PAID-YTD
           END-IF
           ADD ARPM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE ISSUED YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3970-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE ARPM-VOID-AMT ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 12.00 TO ARPM-VOID-AMT
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 15.00 TO ARPM-VOID-AMT
               WHEN OTHER
                   COMPUTE ARPM-VOID-AMT ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-VOID-AMT > 0 AND ARPM-VOID-AMT < 5.00
               MOVE 5.00 TO ARPM-VOID-AMT
           END-IF
           IF ARPM-VOID-AMT > 100.00
               MOVE 100.00 TO ARPM-VOID-AMT
           END-IF
           ADD ARPM-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FEE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3980-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ARPM-FEE-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ARPM-FEE-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-OUTSTAND.
           IF ARPM-OUTSTAND-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-OUTSTAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-ISSUED.
           IF ARPT-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-ISSUED-AMT * ARPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4080-FLAG-OUTSTAND.
           IF ARPT-OUTSTAND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPT-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN OPEN DATE IS PAST
      * SW -> UA WHEN OPEN DATE IS PAST
      * UA -> WB WHEN OPEN DATE IS PAST
      * WB -> ME WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RECON TYPE CD:
      * EEH -> KBS WHEN NEXT DUE DATE IS PAST
      * KBS -> PAC WHEN NEXT DUE DATE IS PAST
      * PAC -> VKT WHEN NEXT DUE DATE IS PAST
      * VKT -> YMX WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KBS' TO ARPM-RECON-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KBS'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAC' TO ARPM-RECON-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAC'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VKT' TO ARPM-RECON-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VKT'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YMX' TO ARPM-RECON-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4130-FLAG-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-ISSUED-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-FEE.
           IF ARPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-FEE-AMT * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-YTD - (WS-WORK-AMT * 2)
           COMPUTE ARPM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4170-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STOP YTD FEE ON THE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4180-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ARPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ARPM-STOP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE ARPM-STOP-YTD ROUNDED = ARPT-FEE-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO ARPM-STOP-YTD
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO ARPM-STOP-YTD
               WHEN OTHER
                   COMPUTE ARPM-STOP-YTD ROUNDED = ARPT-FEE-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-STOP-YTD > 0 AND ARPM-STOP-YTD < 10.00
               MOVE 10.00 TO ARPM-STOP-YTD
           END-IF
           IF ARPM-STOP-YTD > 75.00
               MOVE 75.00 TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E772 ADDR REQUIRED
      * E672 FEE AMT NEGATIVE
      * E509 FEE AMT OVER 50000
      * E909 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT < 0
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT > 50000.00
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN ARPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-STOP.
           IF ARPM-STOP-AMT < 50.00
               MOVE 50.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 10000.00
               MOVE 10000.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4220-FLAG-ISSUED.
           IF ARPT-ISSUED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPT-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 6
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4250-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4260-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-PAID-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE PAID YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4280-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPT-FEE-AMT ROUNDED = ARPM-PAID-YTD *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 7.00 TO ARPT-FEE-AMT
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 15.00 TO ARPT-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPT-FEE-AMT ROUNDED = ARPM-PAID-YTD *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPT-FEE-AMT > 0 AND ARPT-FEE-AMT < 2.00
               MOVE 2.00 TO ARPT-FEE-AMT
           END-IF
           IF ARPT-FEE-AMT > 100.00
               MOVE 100.00 TO ARPT-FEE-AMT
           END-IF
           ADD ARPT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARPM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4300-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STOP YTD.
      *----------------------------------------------------------------*
       4310-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-YTD ROUNDED =
                   ARPM-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4320-LIMIT-VOID.
           IF ARPM-VOID-AMT < 100.00
               MOVE 100.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-VOID-AMT > 500.00
               MOVE 500.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4330-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4340-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4350-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ARPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE ARPM-FEE-AMT ROUNDED = WS-WORK-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-SOURCE-CD = 'GW'
                   MOVE 7.00 TO ARPM-FEE-AMT
               WHEN ARPM-SOURCE-CD = 'MB'
                   MOVE 7.00 TO ARPM-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPM-FEE-AMT ROUNDED = WS-WORK-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-AMT > 0 AND ARPM-FEE-AMT < 10.00
               MOVE 10.00 TO ARPM-FEE-AMT
           END-IF
           IF ARPM-FEE-AMT > 75.00
               MOVE 75.00 TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-ARRISK.
           INITIALIZE ARRISK-PARMS
           MOVE ARPM-ISSUED-YTD
             TO ARRISK-ISSUED-YTD
           MOVE ARPM-PAID-YTD
             TO ARRISK-PAID-YTD
           MOVE ARPM-VOID-YTD
             TO ARRISK-VOID-YTD
           MOVE ARPM-STOP-YTD
             TO ARRISK-STOP-YTD
           MOVE ARPM-ISSUED-AMT
             TO ARRISK-ISSUED-AMT
           MOVE ARPM-OUTSTAND-YTD
             TO ARRISK-OUTSTAND-YTD
           MOVE ARPM-LAST-STMT-DATE
             TO ARRISK-LAST-STMT-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARRISK-NEXT-REVIEW-DATE
           MOVE ARPM-LAST-PAY-DATE
             TO ARRISK-LAST-PAY-DATE
           MOVE ARPM-NEXT-DUE-DATE
             TO ARRISK-NEXT-DUE-DATE
           MOVE ARPM-CYCLE-CD
             TO ARRISK-CYCLE-CD
           MOVE ARPM-CHANNEL-CD
             TO ARRISK-CHANNEL-CD
           MOVE ARPM-REGION-CD
             TO ARRISK-REGION-CD
           MOVE ARPM-BRANCH-CD
             TO ARRISK-BRANCH-CD
           MOVE ARPM-NOTICE-SENT-FLAG
             TO ARRISK-NOTICE-SENT-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARRISK-STOP-FLAG
           MOVE ARPM-HOLD-FLAG
             TO ARRISK-HOLD-FLAG
           MOVE ARPM-ITEM-CNT
             TO ARRISK-ITEM-CNT
           MOVE ARPM-CYCLE-CNT
             TO ARRISK-CYCLE-CNT
           MOVE ARPM-PENALTY-PCT
             TO ARRISK-PENALTY-PCT
           MOVE ARPM-DISCOUNT
             TO ARRISK-DISCOUNT
           MOVE ARPM-ADDR
             TO ARRISK-ADDR
           MOVE ARPM-DESC
             TO ARRISK-DESC
           CALL 'ARRISK' USING ARRISK-PARMS
           EVALUATE ARRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARRISK-VOID-YTD
                     TO ARPM-VOID-YTD
                   MOVE ARRISK-ISSUED-AMT
                     TO ARPM-ISSUED-AMT
                   MOVE ARRISK-OUTSTAND-YTD
                     TO ARPM-OUTSTAND-YTD
                   MOVE ARRISK-NOTICE-SENT-FLAG
                     TO ARPM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-ARRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-ARDTE.
           INITIALIZE ARDTE-PARMS
           MOVE ARPM-FEE-YTD
             TO ARDTE-FEE-YTD
           MOVE ARPM-ISSUED-YTD
             TO ARDTE-ISSUED-YTD
           MOVE ARPM-PAID-YTD
             TO ARDTE-PAID-YTD
           MOVE ARPM-VOID-YTD
             TO ARDTE-VOID-YTD
           MOVE ARPM-FEE-AMT
             TO ARDTE-FEE-AMT
           MOVE ARPM-VOID-AMT
             TO ARDTE-VOID-AMT
           MOVE ARPM-CLOSE-DATE
             TO ARDTE-CLOSE-DATE
           MOVE ARPM-LAST-MAINT-DATE
             TO ARDTE-LAST-MAINT-DATE
           MOVE ARPM-LAST-STMT-DATE
             TO ARDTE-LAST-STMT-DATE
           MOVE ARPM-LAST-PAY-DATE
             TO ARDTE-LAST-PAY-DATE
           MOVE ARPM-SOURCE-CD
             TO ARDTE-SOURCE-CD
           MOVE ARPM-RECON-TYPE-CD
             TO ARDTE-RECON-TYPE-CD
           MOVE ARPM-REGION-CD
             TO ARDTE-REGION-CD
           MOVE ARPM-CYCLE-CD
             TO ARDTE-CYCLE-CD
           MOVE ARPM-EXEMPT-FLAG
             TO ARDTE-EXEMPT-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARDTE-PREFERRED-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARDTE-STOP-FLAG
           MOVE ARPM-ITEM-CNT
             TO ARDTE-ITEM-CNT
           MOVE ARPM-CYCLE-CNT
             TO ARDTE-CYCLE-CNT
           MOVE ARPM-DISCOUNT
             TO ARDTE-DISCOUNT
           MOVE ARPM-PENALTY-PCT
             TO ARDTE-PENALTY-PCT
           MOVE ARPM-CUST-ID
             TO ARDTE-CUST-ID
           MOVE ARPM-NAME
             TO ARDTE-NAME
           CALL 'ARDTE' USING ARDTE-PARMS
           EVALUATE ARDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARDTE-ISSUED-YTD
                     TO ARPM-ISSUED-YTD
                   MOVE ARDTE-PAID-YTD
                     TO ARPM-PAID-YTD
                   MOVE ARDTE-VOID-YTD
                     TO ARPM-VOID-YTD
                   MOVE ARDTE-VOID-AMT
                     TO ARPM-VOID-AMT
                   MOVE ARDTE-STOP-FLAG
                     TO ARPM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-ARDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-ARPRC.
           INITIALIZE ARPRC-PARMS
           MOVE ARPM-OUTSTAND-YTD
             TO ARPRC-OUTSTAND-YTD
           MOVE ARPM-ISSUED-AMT
             TO ARPRC-ISSUED-AMT
           MOVE ARPM-ISSUED-YTD
             TO ARPRC-ISSUED-YTD
           MOVE ARPM-FEE-YTD
             TO ARPRC-FEE-YTD
           MOVE ARPM-OUTSTAND-AMT
             TO ARPRC-OUTSTAND-AMT
           MOVE ARPM-STOP-YTD
             TO ARPRC-STOP-YTD
           MOVE ARPM-OPEN-DATE
             TO ARPRC-OPEN-DATE
           MOVE ARPM-NEXT-DUE-DATE
             TO ARPRC-NEXT-DUE-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARPRC-NEXT-REVIEW-DATE
           MOVE ARPM-LAST-ACT-DATE
             TO ARPRC-LAST-ACT-DATE
           MOVE ARPM-RECON-TYPE-CD
             TO ARPRC-RECON-TYPE-CD
           MOVE ARPM-BRANCH-CD
             TO ARPRC-BRANCH-CD
           MOVE ARPM-CHANNEL-CD
             TO ARPRC-CHANNEL-CD
           MOVE ARPM-SOURCE-CD
             TO ARPRC-SOURCE-CD
           MOVE ARPM-HOLD-FLAG
             TO ARPRC-HOLD-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARPRC-STOP-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARPRC-PREFERRED-FLAG
           MOVE ARPM-PAYMENT-CNT
             TO ARPRC-PAYMENT-CNT
           MOVE ARPM-EXCEPT-CNT
             TO ARPRC-EXCEPT-CNT
           MOVE ARPM-DISCOUNT
             TO ARPRC-DISCOUNT
           MOVE ARPM-PENALTY-PCT
             TO ARPRC-PENALTY-PCT
           MOVE ARPM-NAME
             TO ARPRC-NAME
           MOVE ARPM-ADDR
             TO ARPRC-ADDR
           CALL 'ARPRC' USING ARPRC-PARMS
           EVALUATE ARPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARPRC-ISSUED-AMT
                     TO ARPM-ISSUED-AMT
                   MOVE ARPRC-ISSUED-YTD
                     TO ARPM-ISSUED-YTD
                   MOVE ARPRC-OUTSTAND-AMT
                     TO ARPM-OUTSTAND-AMT
                   MOVE ARPRC-STOP-YTD
                     TO ARPM-STOP-YTD
                   MOVE ARPRC-HOLD-FLAG
                     TO ARPM-HOLD-FLAG
                   MOVE ARPRC-STOP-FLAG
                     TO ARPM-STOP-FLAG
                   MOVE ARPRC-PREFERRED-FLAG
                     TO ARPM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ARPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ARPT-KEY TO RD-KEY
           MOVE ARPT-TRAN-CD TO RD-CODE
           MOVE ARPT-OUTSTAND-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ARPT-KEY TO RD-KEY
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
           DISPLAY 'ARPPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ARPPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
