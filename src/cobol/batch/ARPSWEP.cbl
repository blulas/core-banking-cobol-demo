      *================================================================*
      * PROGRAM  : ARPSWEP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ACCOUNT RECONCILEMENT - SWEEP. READS THE ARP        *
      *            TRANSACTIONS, UPDATES THE ARP MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FULL AND PARTIAL ACCOUNT        *
      *            RECONCILEMENT SERVICE FOR CORPORATES                *
      * INPUT    : ARPMST (KSDS), ARPTRN, BANKCTL                      *
      * OUTPUT   : ARPOUT, ARPSWRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ARPRC, ARDTE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-27 PQR  ORIGINAL                                       *
      * 2001-08-28 RJT  STOP LIMIT REVISED CHG5371                     *
      * 2003-09-17 DPS  PAID NOTICE TEXT PER LEGAL INC4332             *
      * 2005-04-22 EWS  STOP CALCULATION CORRECTED INC9492             *
      * 2005-10-16 GAF  STOP EXCEPTIONS TO OPS QUEUE PRB7623           *
      * 2009-12-02 LMB  STOP CUTOFF MOVED INC6728                      *
      * 2010-11-24 TWH  AUDIT FINDING - VOID CONTROL TOTALS CHG2761    *
      * 2011-11-02 CRP  ISSUED LIMIT REVISED PRB4199                   *
      * 2012-01-05 GAF  VOID CUTOFF MOVED REQ1463                      *
      * 2012-01-17 EWS  PAID STATUS CODES ADDED CHG6968                *
      * 2014-08-06 SKN  VOID TABLE EXPANDED INC5191                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPSWEP.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "ARPSWRPT"
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
                                           VALUE 11.000000.
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
      * WS-PAID-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000145500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000554500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000686000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000676500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021500000000772000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000307000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034000000000511500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036500000000561000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036750000000339000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036850000000285000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036950000000425000'.
       01  WS-PAID-GRID-TBL REDEFINES WS-PAID-GRID-VALUES.
           05  WS-PAID-GRID-ROW OCCURS 11 TIMES.
               10  WS-PAID-GRID-LOW        PIC 9(09)V99.
               10  WS-PAID-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ISSUED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000438000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000288000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000878000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000322000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000520500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026050000000203500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028550000000184500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029050000000878000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034050000000177000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039050000000063000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00049050000000089000'.
       01  WS-ISSUED-TIER-TBL REDEFINES WS-ISSUED-TIER-VALUES.
           05  WS-ISSUED-TIER-ROW OCCURS 12 TIMES.
               10  WS-ISSUED-TIER-LOW      PIC 9(09)V99.
               10  WS-ISSUED-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STOP-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STOP-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000355000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000453500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000769000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000467500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000433000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000176000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000775000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011200000000550000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000277000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000194000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000306000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000384500'.
       01  WS-STOP-BAND-TBL REDEFINES WS-STOP-BAND-VALUES.
           05  WS-STOP-BAND-ROW OCCURS 12 TIMES.
               10  WS-STOP-BAND-LOW        PIC 9(09)V99.
               10  WS-STOP-BAND-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ARPSWEP  ACCOUNT RECONCILEMENT'.
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
       COPY ARPRCP.
       COPY ARDTEP.

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
               PERFORM 3000-PRORATE-FEE
               PERFORM 3010-SELECT-FEE-PCT
               PERFORM 3020-PRORATE-OUTSTAND
               PERFORM 3030-DESCRIBE-CHANNEL
               PERFORM 3040-SELECT-PENALTY-PCT
               PERFORM 3050-SELECT-PENALTY-PCT
               PERFORM 3060-PRORATE-OUTSTAND
               PERFORM 3070-ACCUM-REGION
               PERFORM 3080-ASSESS-STOP
               PERFORM 3090-PRORATE-STOP
               PERFORM 3100-ACCRUE-ISSUED
               PERFORM 3110-EDIT-DESC
               PERFORM 3120-PRORATE-VOID
               PERFORM 3130-AGE-NEXT-DUE
               PERFORM 3140-EDIT-REJECT
               PERFORM 3150-CHECK-NEXT-REVIEW
               PERFORM 3160-ASSESS-HOLD
               PERFORM 3170-PRORATE-OUTSTAND
               PERFORM 3180-ACCUM-BRANCH
               PERFORM 3190-ACCUM-TRAN
               PERFORM 3200-SPLIT-VOID
               PERFORM 3210-SELECT-PENALTY-PCT
               PERFORM 3220-ACCUM-TRAN
               PERFORM 3230-LIMIT-OUTSTAND
               PERFORM 3240-ACCUM-TRAN
               PERFORM 3250-DESCRIBE-REGION
               PERFORM 3260-SELECT-FEE-PCT
               PERFORM 3270-ACCRUE-OUTSTAND
               PERFORM 3280-FLAG-FEE
               PERFORM 3290-AGE-LAST-MAINT
               PERFORM 3300-EDIT-REF-NO
               PERFORM 3310-SET-TRAN
               PERFORM 3320-ACCUM-CHANNEL
               PERFORM 3330-SPLIT-PAID
               PERFORM 3340-FLAG-VOID
               PERFORM 3350-SELECT-DISCOUNT
               PERFORM 3360-SPLIT-OUTSTAND
               PERFORM 3370-FLAG-PAID
               PERFORM 3380-SET-SOURCE
               PERFORM 3390-CHECK-NEXT-REVIEW
               PERFORM 3400-LIMIT-PAID
               PERFORM 3410-EDIT-DESC
               PERFORM 3420-PRORATE-PAID
               PERFORM 3430-ACCRUE-VOID
               PERFORM 3440-DESCRIBE-TRAN
               PERFORM 3450-COUNT-EXCEPT
               PERFORM 3460-PRORATE-OUTSTAND
               PERFORM 3470-SET-CHANNEL
               PERFORM 3480-ACCRUE-OUTSTAND
               PERFORM 3490-ACCRUE-STOP
               PERFORM 3500-ACCUM-CHANNEL
               PERFORM 3510-ACCUM-RECON-TYPE
               PERFORM 3520-FLAG-VOID
               PERFORM 3530-DESCRIBE-CHANNEL
               PERFORM 3540-SELECT-PENALTY-PCT
               PERFORM 3550-FLAG-PAID
               PERFORM 3560-AGE-NEXT-REVIEW
               PERFORM 3570-COUNT-SEQ
               PERFORM 3580-CHECK-LAST-MAINT
               PERFORM 3590-FLAG-PAID
               PERFORM 3600-SPLIT-STOP
               PERFORM 3610-SELECT-PENALTY-PCT
               PERFORM 3620-ACCUM-CHANNEL
               PERFORM 3630-ACCRUE-FEE
               PERFORM 3640-SET-BRANCH
               PERFORM 3650-SELECT-DISCOUNT
               PERFORM 3660-SET-TRAN
               PERFORM 3670-COUNT-ITEM
               PERFORM 3680-PRORATE-STOP
               PERFORM 3690-LIMIT-ISSUED
               PERFORM 3700-FLAG-OUTSTAND
               PERFORM 3710-PRORATE-STOP
               PERFORM 3720-ACCUM-CYCLE
               PERFORM 3730-LIMIT-VOID
               PERFORM 3740-FLAG-VOID
               PERFORM 3750-ASSESS-PREFERRED
               PERFORM 3760-LIMIT-STOP
               PERFORM 3770-FLAG-ITEM
               PERFORM 3780-CHECK-LAST-ACT
               PERFORM 3790-ACCUM-REGION
               PERFORM 3800-COUNT-PAYMENT
               PERFORM 3810-LIMIT-VOID
               PERFORM 3820-EDIT-CONTACT
               PERFORM 3830-DESCRIBE-RECON-TYPE
               PERFORM 3840-EDIT-CONTACT
               PERFORM 3850-CHECK-CLOSE
               PERFORM 3860-CHECK-LAST-PAY
               PERFORM 3870-CHECK-OPEN
               PERFORM 3880-ACCRUE-FEE
               PERFORM 3890-ASSESS-PREFERRED
               PERFORM 3900-SPLIT-VOID
               PERFORM 3910-SELECT-FEE-PCT
               PERFORM 3920-ACCRUE-VOID
               PERFORM 3930-PRORATE-PAID
               PERFORM 3940-SPLIT-PAID
               PERFORM 3950-PRORATE-STOP
               PERFORM 3960-EDIT-NAME
               PERFORM 3970-EDIT-DESC
               PERFORM 3980-COUNT-ITEM
               PERFORM 3990-SET-SOURCE
               PERFORM 4000-PRORATE-STOP
               PERFORM 4010-EDIT-REJECT
               PERFORM 4020-SPLIT-OUTSTAND
               PERFORM 4030-ACCRUE-FEE
               PERFORM 4040-ACCUM-RECON-TYPE
               PERFORM 4050-SET-BRANCH
               PERFORM 4060-SPLIT-STOP
               PERFORM 4070-CHECK-NEXT-REVIEW
               PERFORM 4080-ASSESS-OVERRIDE
               PERFORM 4090-EDIT-CONTACT
               PERFORM 4100-COUNT-CYCLE
               PERFORM 4110-DESCRIBE-TRAN
               PERFORM 4120-ACCRUE-VOID
               PERFORM 4130-LIMIT-VOID
               PERFORM 4140-SPLIT-PAID
               PERFORM 4150-ASSESS-EXEMPT
               PERFORM 4160-PRORATE-VOID
               PERFORM 4170-SPLIT-ISSUED
               PERFORM 4180-ACCRUE-OUTSTAND
               PERFORM 4190-COUNT-ITEM
               PERFORM 4200-PRORATE-VOID
               PERFORM 7000-CALL-ARPRC
               PERFORM 7020-CALL-ARDTE
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
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-TIER-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPT-FEE-AMT NOT < WS-ISSUED-TIER-LOW(WS-TX)
                   MOVE WS-ISSUED-TIER-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3020-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   ARPM-VOID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED TIER TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPM-PAID-AMT NOT < WS-ISSUED-TIER-LOW(WS-TX)
                   MOVE WS-ISSUED-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STOP BAND TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STOP-BAND-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPM-VOID-AMT NOT < WS-STOP-BAND-LOW(WS-TX)
                   MOVE WS-STOP-BAND-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3060-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   ARPM-VOID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE VOID AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3080-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ARPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ARPT-VOID-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ARPT-VOID-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STOP AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-AMT ROUNDED =
                   ARPM-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-STOP-AMT
           END-IF
           ADD ARPM-STOP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3100-ACCRUE-ISSUED.
           IF ARPM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-AMT * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E453 DESC REQUIRED
      * E984 FEE AMT NEGATIVE
      * E461 FEE AMT OVER 99999
      * E361 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT < 0
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT > 99999.00
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN ARPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-VOID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-NEXT-DUE.
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
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E368 REJECT CD REQUIRED
      * E371 ISSUED AMT NEGATIVE
      * E573 ISSUED AMT OVER 99999
      * E002 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN ARPT-ISSUED-AMT < 0
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN ARPT-ISSUED-AMT > 99999.00
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE STOP YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3160-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPM-STOP-YTD *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 5.00 TO ARPM-FEE-AMT
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 12.00 TO ARPM-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPM-STOP-YTD *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-AMT > 0 AND ARPM-FEE-AMT < 2.00
               MOVE 2.00 TO ARPM-FEE-AMT
           END-IF
           IF ARPM-FEE-AMT > 25.00
               MOVE 25.00 TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3170-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   ARPM-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-BRANCH-CD
                   ADD ARPT-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD ARPM-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPT-VOID-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPT-VOID-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID GRID TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-GRID-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPM-PAID-AMT NOT < WS-PAID-GRID-LOW(WS-TX)
                   MOVE WS-PAID-GRID-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STOP YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD ARPM-STOP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-STOP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-OUTSTAND.
           IF ARPM-OUTSTAND-AMT < 100.00
               MOVE 100.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-OUTSTAND-AMT > 50000.00
               MOVE 50000.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OUTSTAND AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD ARPT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-OUTSTAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID GRID TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-GRID-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPM-PAID-AMT NOT < WS-PAID-GRID-LOW(WS-TX)
                   MOVE WS-PAID-GRID-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-YTD * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3280-FLAG-FEE.
           IF ARPM-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E774 REF NO REQUIRED
      * E711 OUTSTAND AMT NEGATIVE
      * E786 OUTSTAND AMT OVER 10000
      * E076 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-REF-NO = SPACES
                   MOVE 'E774' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-AMT < 0
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-AMT > 10000.00
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN NEXT REVIEW DATE IS PAST
      * PD -> SP WHEN NEXT REVIEW DATE IS PAST
      * SP -> VD WHEN NEXT REVIEW DATE IS PAST
      * VD -> FE WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FE' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-CHANNEL-CD
                   ADD ARPM-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3330-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARPM-PAID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-PAID-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-STOP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-VOID.
           IF ARPM-VOID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STOP BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STOP-BAND-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPT-FEE-AMT NOT < WS-STOP-BAND-LOW(WS-TX)
                   MOVE WS-STOP-BAND-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3370-FLAG-PAID.
           IF ARPM-PAID-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN LAST ACT DATE IS PAST
      * MB -> MH WHEN LAST ACT DATE IS PAST
      * MH -> UG WHEN LAST ACT DATE IS PAST
      * UG -> UR WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO ARPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3400-LIMIT-PAID.
           IF ARPM-PAID-AMT < 100.00
               MOVE 100.00 TO ARPM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-PAID-AMT > 2500.00
               MOVE 2500.00 TO ARPM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E637 DESC REQUIRED
      * E502 OUTSTAND YTD NEGATIVE
      * E276 OUTSTAND YTD OVER 1000000
      * E277 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E502' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 1000000.00
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3420-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-PAID-YTD
           END-IF
           ADD ARPM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-VOID.
           IF ARPM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-YTD * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-STOP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ARPM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3460-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   ARPM-ISSUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-YTD * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-STOP.
           IF ARPM-STOP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-YTD * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD ARPM-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-RECON-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-VOID.
           IF ARPT-VOID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPT-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPM-FEE-AMT NOT < WS-ISSUED-TIER-LOW(WS-TX)
                   MOVE WS-ISSUED-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3550-FLAG-PAID.
           IF ARPM-PAID-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0 AND ARPM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 10
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3590-FLAG-PAID.
           IF ARPM-PAID-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE ARPM-STOP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID GRID TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-GRID-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPT-ISSUED-AMT NOT < WS-PAID-GRID-LOW(WS-TX)
                   MOVE WS-PAID-GRID-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-CHANNEL-CD
                   ADD ARPM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-FEE.
           IF ARPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-FEE-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPT-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN EFF DATE IS PAST
      * GM -> GV WHEN EFF DATE IS PAST
      * GV -> HK WHEN EFF DATE IS PAST
      * HK -> VX WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STOP BAND TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STOP-BAND-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPT-ISSUED-AMT NOT < WS-STOP-BAND-LOW(WS-TX)
                   MOVE WS-STOP-BAND-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN NEXT DUE DATE IS PAST
      * PD -> SP WHEN NEXT DUE DATE IS PAST
      * SP -> VD WHEN NEXT DUE DATE IS PAST
      * VD -> FE WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO ARPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STOP AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-AMT ROUNDED =
                   ARPM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-STOP-AMT
           END-IF
           ADD ARPM-STOP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-ISSUED.
           IF ARPM-ISSUED-AMT < 10.00
               MOVE 10.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-AMT > 5000.00
               MOVE 5000.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3700-FLAG-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STOP YTD.
      *----------------------------------------------------------------*
       3710-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-YTD ROUNDED =
                   ARPM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-CYCLE.
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
      * VOID AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-VOID.
           IF ARPM-VOID-AMT < 100.00
               MOVE 100.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-VOID-AMT > 50000.00
               MOVE 50000.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3740-FLAG-VOID.
           IF ARPM-VOID-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID YTD FEE ON THE PAID YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3750-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-VOID-YTD ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 15.00 TO ARPM-VOID-YTD
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 15.00 TO ARPM-VOID-YTD
               WHEN OTHER
                   COMPUTE ARPM-VOID-YTD ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-VOID-YTD > 0 AND ARPM-VOID-YTD < 10.00
               MOVE 10.00 TO ARPM-VOID-YTD
           END-IF
           IF ARPM-VOID-YTD > 250.00
               MOVE 250.00 TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3760-LIMIT-STOP.
           IF ARPM-STOP-AMT < 10.00
               MOVE 10.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 500.00
               MOVE 500.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3770-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-LAST-ACT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-REGION-CD
                   ADD ARPM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO ARPM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-VOID.
           IF ARPT-VOID-AMT < 50.00
               MOVE 50.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 10000.00
               MOVE 10000.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E337 CONTACT REQUIRED
      * E485 VOID YTD NEGATIVE
      * E755 VOID YTD OVER 1000000
      * E465 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN ARPM-VOID-YTD < 0
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN ARPM-VOID-YTD > 1000000.00
                   MOVE 'E755' TO WS-REJECT-CODE
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
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E643 CONTACT REQUIRED
      * E893 FEE AMT NEGATIVE
      * E750 FEE AMT OVER 5000
      * E163 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT < 0
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT > 5000.00
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN ARPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-CLOSE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-LAST-PAY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO ARPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-OPEN.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-FEE.
           IF ARPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-FEE-AMT * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3890-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-STOP-FLAG = 'Y'
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPM-OUTSTAND-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO ARPM-FEE-YTD
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO ARPM-FEE-YTD
               WHEN OTHER
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPM-OUTSTAND-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-YTD > 0 AND ARPM-FEE-YTD < 10.00
               MOVE 10.00 TO ARPM-FEE-YTD
           END-IF
           IF ARPM-FEE-YTD > 50.00
               MOVE 50.00 TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STOP BAND TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STOP-BAND-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ARPM-VOID-YTD NOT < WS-STOP-BAND-LOW(WS-TX)
                   MOVE WS-STOP-BAND-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-VOID.
           IF ARPM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-YTD * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STOP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-AMT ROUNDED =
                   ARPM-STOP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARPM-PAID-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-PAID-YTD - (WS-WORK-AMT * 2)
           COMPUTE ARPM-STOP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STOP YTD.
      *----------------------------------------------------------------*
       3950-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-YTD ROUNDED =
                   ARPM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E397 NAME REQUIRED
      * E579 OUTSTAND AMT NEGATIVE
      * E746 OUTSTAND AMT OVER 250000
      * E639 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-NAME = SPACES
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT < 0
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT > 250000.00
                   MOVE 'E746' TO WS-REJECT-CODE
               WHEN ARPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E567 DESC REQUIRED
      * E450 ISSUED YTD NEGATIVE
      * E459 ISSUED YTD OVER 50000
      * E960 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD < 0
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD > 50000.00
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN ARPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN POST DATE IS PAST
      * MB -> MH WHEN POST DATE IS PAST
      * MH -> UG WHEN POST DATE IS PAST
      * UG -> UR WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STOP YTD.
      *----------------------------------------------------------------*
       4000-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-YTD ROUNDED =
                   ARPM-VOID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E848 REJECT CD REQUIRED
      * E730 WORK AMT NEGATIVE
      * E185 WORK AMT OVER 1000000
      * E534 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E185' TO WS-REJECT-CODE
               WHEN ARPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPT-OUTSTAND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPT-OUTSTAND-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-FEE.
           IF ARPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-FEE-AMT * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-STOP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-RECON-TYPE-CD
                   ADD ARPM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN OPEN DATE IS PAST
      * GM -> GV WHEN OPEN DATE IS PAST
      * GV -> HK WHEN OPEN DATE IS PAST
      * HK -> VX WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STOP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARPM-STOP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-STOP-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARPM-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4080-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ARPM-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-ISSUED-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO ARPM-OUTSTAND-YTD
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO ARPM-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-ISSUED-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-OUTSTAND-YTD > 0 AND ARPM-OUTSTAND-YTD < 5.00
               MOVE 5.00 TO ARPM-OUTSTAND-YTD
           END-IF
           IF ARPM-OUTSTAND-YTD > 250.00
               MOVE 250.00 TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E618 CONTACT REQUIRED
      * E827 WORK AMT NEGATIVE
      * E751 WORK AMT OVER 1000000
      * E413 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E751' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ARPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-VOID.
           IF ARPT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-VOID-AMT * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-VOID.
           IF ARPT-VOID-AMT < 0.00
               MOVE 0.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 50000.00
               MOVE 50000.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARPM-PAID-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-PAID-YTD - (WS-WORK-AMT * 2)
           COMPUTE ARPT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4150-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPT-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   COMPUTE ARPT-ISSUED-AMT ROUNDED = ARPM-FEE-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-CHANNEL-CD = 'ME'
                   MOVE 5.00 TO ARPT-ISSUED-AMT
               WHEN ARPM-CHANNEL-CD = 'SW'
                   MOVE 12.00 TO ARPT-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPT-ISSUED-AMT ROUNDED = ARPM-FEE-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPT-ISSUED-AMT > 0 AND ARPT-ISSUED-AMT < 1.00
               MOVE 1.00 TO ARPT-ISSUED-AMT
           END-IF
           IF ARPT-ISSUED-AMT > 25.00
               MOVE 25.00 TO ARPT-ISSUED-AMT
           END-IF
           ADD ARPT-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       4160-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-VOID-AMT ROUNDED =
                   ARPM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPT-VOID-AMT
           END-IF
           ADD ARPT-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPT-ISSUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPT-ISSUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4190-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       4200-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-VOID-AMT ROUNDED =
                   ARPM-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-VOID-AMT
           END-IF
           ADD ARPM-VOID-AMT TO WS-TOTAL-PRORATED.

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
                   MOVE ARPRC-OUTSTAND-YTD
                     TO ARPM-OUTSTAND-YTD
                   MOVE ARPRC-ISSUED-AMT
                     TO ARPM-ISSUED-AMT
                   MOVE ARPRC-ISSUED-YTD
                     TO ARPM-ISSUED-YTD
                   MOVE ARPRC-FEE-YTD
                     TO ARPM-FEE-YTD
                   MOVE ARPRC-OUTSTAND-AMT
                     TO ARPM-OUTSTAND-AMT
                   MOVE ARPRC-HOLD-FLAG
                     TO ARPM-HOLD-FLAG
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
                   MOVE ARDTE-FEE-AMT
                     TO ARPM-FEE-AMT
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
           DISPLAY 'ARPSWEP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ARPSWEP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
