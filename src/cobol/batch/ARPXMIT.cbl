      *================================================================*
      * PROGRAM  : ARPXMIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ACCOUNT RECONCILEMENT - OUTBOUND TRANSMISSION. READS*
      *            THE ARP TRANSACTIONS, UPDATES THE ARP MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. FULL AND PARTIAL       *
      *            ACCOUNT RECONCILEMENT SERVICE FOR CORPORATES        *
      * INPUT    : ARPMST (KSDS), ARPTRN, BANKCTL                      *
      * OUTPUT   : ARPOUT, ARPXMRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ARDTE, ARSTAT, ARPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-12-24 GAF  ORIGINAL                                       *
      * 2001-09-17 RJT  FEE EXCEPTIONS TO OPS QUEUE PRB9100            *
      * 2002-07-19 LMB  ISSUED TABLE EXPANDED INC5398                  *
      * 2006-08-12 PQR  OUTSTAND CUTOFF MOVED CHG2551                  *
      * 2010-07-17 TWH  AUDIT FINDING - PAID CONTROL TOTALS INC8453    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPXMIT.
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
           SELECT REPORT-FILE ASSIGN TO "ARPXMRPT"
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
                                           VALUE 8.000000.
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
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000632000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000025000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000556500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000153500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002350000000684000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004850000000233000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000132500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000896000'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 8 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-OUTSTAND-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000873000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000253500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000400500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000485000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000134000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018100000000412500'.
       01  WS-OUTSTAND-TIER-TBL REDEFINES WS-OUTSTAND-TIER-VALUES.
           05  WS-OUTSTAND-TIER-ROW OCCURS 6 TIMES.
               10  WS-OUTSTAND-TIER-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000833000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000489000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000771500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000663000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000864000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011400000000085000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016400000000498000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016650000000406000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017650000000881000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000620000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000585000'.
       01  WS-FEE-TIERX-TBL REDEFINES WS-FEE-TIERX-VALUES.
           05  WS-FEE-TIERX-ROW OCCURS 11 TIMES.
               10  WS-FEE-TIERX-LOW        PIC 9(09)V99.
               10  WS-FEE-TIERX-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ARPXMIT  ACCOUNT RECONCILEMENT'.
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
       COPY ARDTEP.
       COPY ARSTATP.
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
               PERFORM 3000-FLAG-FEE
               PERFORM 3010-ACCRUE-PAID
               PERFORM 3020-DESCRIBE-BRANCH
               PERFORM 3030-SELECT-FEE-PCT
               PERFORM 3040-DESCRIBE-RECON-TYPE
               PERFORM 3050-SPLIT-STOP
               PERFORM 3060-SPLIT-PAID
               PERFORM 3070-ACCUM-CHANNEL
               PERFORM 3080-SPLIT-ISSUED
               PERFORM 3090-FLAG-ISSUED
               PERFORM 3100-ACCUM-RECON-TYPE
               PERFORM 3110-DESCRIBE-REGION
               PERFORM 3120-COUNT-CYCLE
               PERFORM 3130-SET-SOURCE
               PERFORM 3140-AGE-LAST-MAINT
               PERFORM 3150-CHECK-OPEN
               PERFORM 3160-SELECT-PENALTY-PCT
               PERFORM 3170-ACCUM-REGION
               PERFORM 3180-ASSESS-NOTICE-SENT
               PERFORM 3190-DESCRIBE-REGION
               PERFORM 3200-LIMIT-STOP
               PERFORM 3210-FLAG-VOID
               PERFORM 3220-PRORATE-OUTSTAND
               PERFORM 3230-SET-CHANNEL
               PERFORM 3240-FLAG-STOP
               PERFORM 3250-LIMIT-FEE
               PERFORM 3260-SELECT-DISCOUNT
               PERFORM 3270-EDIT-MEMO
               PERFORM 3280-SELECT-DISCOUNT
               PERFORM 3290-ACCUM-TRAN
               PERFORM 3300-SET-SOURCE
               PERFORM 3310-SELECT-DISCOUNT
               PERFORM 3320-SET-CHANNEL
               PERFORM 3330-EDIT-OPERATOR
               PERFORM 3340-CHECK-LAST-MAINT
               PERFORM 3350-CHECK-NEXT-DUE
               PERFORM 3360-FLAG-STOP
               PERFORM 3370-EDIT-MEMO
               PERFORM 3380-EDIT-MEMO
               PERFORM 3390-PRORATE-ISSUED
               PERFORM 3400-CHECK-NEXT-DUE
               PERFORM 3410-SPLIT-OUTSTAND
               PERFORM 3420-COUNT-EXCEPT
               PERFORM 3430-ASSESS-STOP
               PERFORM 3440-SET-SOURCE
               PERFORM 3450-EDIT-MEMO
               PERFORM 3460-DESCRIBE-CYCLE
               PERFORM 3470-SET-SOURCE
               PERFORM 3480-ACCRUE-VOID
               PERFORM 3490-DESCRIBE-SOURCE
               PERFORM 3500-PRORATE-PAID
               PERFORM 3510-ASSESS-OVERRIDE
               PERFORM 3520-LIMIT-STOP
               PERFORM 3530-PRORATE-OUTSTAND
               PERFORM 3540-COUNT-ITEM
               PERFORM 3550-FLAG-OUTSTAND
               PERFORM 3560-SPLIT-FEE
               PERFORM 3570-SPLIT-FEE
               PERFORM 3580-DESCRIBE-REGION
               PERFORM 3590-COUNT-ITEM
               PERFORM 3600-DESCRIBE-RECON-TYPE
               PERFORM 3610-SET-TRAN
               PERFORM 3620-PRORATE-OUTSTAND
               PERFORM 3630-ACCUM-CYCLE
               PERFORM 3640-SPLIT-ISSUED
               PERFORM 3650-EDIT-NAME
               PERFORM 3660-EDIT-CUST-ID
               PERFORM 3670-PRORATE-FEE
               PERFORM 3680-PRORATE-OUTSTAND
               PERFORM 3690-ASSESS-NOTICE-SENT
               PERFORM 3700-DESCRIBE-CHANNEL
               PERFORM 3710-COUNT-EXCEPT
               PERFORM 3720-LIMIT-ITEM
               PERFORM 3730-SELECT-DISCOUNT
               PERFORM 3740-PRORATE-PAID
               PERFORM 3750-COUNT-ITEM
               PERFORM 3760-CHECK-LAST-PAY
               PERFORM 3770-ASSESS-EXEMPT
               PERFORM 3780-FLAG-FEE
               PERFORM 3790-PRORATE-OUTSTAND
               PERFORM 3800-ASSESS-HOLD
               PERFORM 3810-AGE-EFFECTIVE
               PERFORM 3820-SET-CHANNEL
               PERFORM 3830-ACCRUE-OUTSTAND
               PERFORM 3840-AGE-NEXT-DUE
               PERFORM 3850-AGE-NEXT-REVIEW
               PERFORM 3860-ACCUM-CYCLE
               PERFORM 3870-ACCRUE-ISSUED
               PERFORM 3880-ASSESS-HOLD
               PERFORM 3890-ACCUM-BRANCH
               PERFORM 3900-EDIT-CONTACT
               PERFORM 3910-ACCRUE-VOID
               PERFORM 3920-DESCRIBE-CHANNEL
               PERFORM 3930-PRORATE-STOP
               PERFORM 3940-COUNT-ITEM
               PERFORM 3950-PRORATE-ISSUED
               PERFORM 3960-ACCRUE-ISSUED
               PERFORM 3970-SET-CYCLE
               PERFORM 3980-SET-CHANNEL
               PERFORM 3990-SET-CHANNEL
               PERFORM 4000-ACCRUE-VOID
               PERFORM 4010-DESCRIBE-CYCLE
               PERFORM 4020-EDIT-ADDR
               PERFORM 4030-COUNT-EXCEPT
               PERFORM 4040-CHECK-EFFECTIVE
               PERFORM 4050-DESCRIBE-SOURCE
               PERFORM 4060-LIMIT-VOID
               PERFORM 4070-ASSESS-PREFERRED
               PERFORM 4080-DESCRIBE-SOURCE
               PERFORM 4090-FLAG-ITEM
               PERFORM 4100-LIMIT-ISSUED
               PERFORM 4110-ACCRUE-FEE
               PERFORM 4120-CHECK-EFFECTIVE
               PERFORM 4130-FLAG-PAID
               PERFORM 4140-SET-TRAN
               PERFORM 4150-SELECT-FEE-PCT
               PERFORM 4160-CHECK-EFFECTIVE
               PERFORM 4170-AGE-NEXT-REVIEW
               PERFORM 4180-LIMIT-STOP
               PERFORM 4190-DESCRIBE-RECON-TYPE
               PERFORM 4200-AGE-LAST-ACT
               PERFORM 4210-CHECK-NEXT-REVIEW
               PERFORM 4220-EDIT-OPERATOR
               PERFORM 4230-EDIT-REJECT
               PERFORM 4240-ACCRUE-OUTSTAND
               PERFORM 4250-EDIT-DESC
               PERFORM 4260-PRORATE-ISSUED
               PERFORM 4270-EDIT-DESC
               PERFORM 4280-ACCRUE-DISCOUNT
               PERFORM 4290-EDIT-CONTACT
               PERFORM 4300-ACCUM-SOURCE
               PERFORM 4310-LIMIT-OUTSTAND
               PERFORM 4320-SPLIT-ISSUED
               PERFORM 4330-PRORATE-VOID
               PERFORM 4340-DESCRIBE-CHANNEL
               PERFORM 4350-PRORATE-FEE
               PERFORM 4360-ASSESS-OVERRIDE
               PERFORM 4370-SPLIT-VOID
               PERFORM 4380-CHECK-LAST-ACT
               PERFORM 4390-FLAG-FEE
               PERFORM 4400-SET-CHANNEL
               PERFORM 4410-LIMIT-VOID
               PERFORM 4420-DESCRIBE-BRANCH
               PERFORM 4430-LIMIT-VOID
               PERFORM 4440-LIMIT-PAID
               PERFORM 4450-EDIT-CONTACT
               PERFORM 4460-SPLIT-OUTSTAND
               PERFORM 4470-LIMIT-VOID
               PERFORM 4480-DESCRIBE-CHANNEL
               PERFORM 4490-ACCUM-CHANNEL
               PERFORM 4500-FLAG-PAID
               PERFORM 4510-AGE-NEXT-DUE
               PERFORM 4520-PRORATE-OUTSTAND
               PERFORM 4530-FLAG-VOID
               PERFORM 4540-ACCRUE-STOP
               PERFORM 4550-DESCRIBE-BRANCH
               PERFORM 4560-COUNT-PAYMENT
               PERFORM 4570-SPLIT-OUTSTAND
               PERFORM 4580-FLAG-OUTSTAND
               PERFORM 4590-PRORATE-PAID
               PERFORM 4600-PRORATE-PAID
               PERFORM 4610-EDIT-REF-NO
               PERFORM 4620-SELECT-PENALTY-PCT
               PERFORM 4630-CHECK-LAST-ACT
               PERFORM 4640-ACCRUE-OUTSTAND
               PERFORM 4650-SPLIT-OUTSTAND
               PERFORM 4660-PRORATE-OUTSTAND
               PERFORM 4670-SET-SOURCE
               PERFORM 4680-SELECT-FEE-PCT
               PERFORM 4690-CHECK-CLOSE
               PERFORM 7020-CALL-ARDTE
               PERFORM 7010-CALL-ARSTAT
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
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3000-FLAG-FEE.
           IF ARPM-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-PAID.
           IF ARPM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-PAID-YTD * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPM-VOID-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STOP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-STOP.
           COMPUTE WS-WORK-AMT = ARPM-STOP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-STOP-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARPM-PAID-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-PAID-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARPT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CHANNEL.
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
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE ARPM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3090-FLAG-ISSUED.
           IF ARPM-ISSUED-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-ISSUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-RECON-TYPE-CD
                   ADD ARPM-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN EFF DATE IS PAST
      * MB -> MH WHEN EFF DATE IS PAST
      * MH -> UG WHEN EFF DATE IS PAST
      * UG -> UR WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-LAST-MAINT.
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
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-OPEN.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIERX TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIERX-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPT-ISSUED-AMT NOT < WS-FEE-TIERX-LOW(WS-TX)
                   MOVE WS-FEE-TIERX-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-REGION.
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
      * ASSESS THE VOID YTD FEE ON THE ISSUED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3180-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE ARPM-VOID-YTD ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 7.00 TO ARPM-VOID-YTD
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 7.00 TO ARPM-VOID-YTD
               WHEN OTHER
                   COMPUTE ARPM-VOID-YTD ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-VOID-YTD > 0 AND ARPM-VOID-YTD < 5.00
               MOVE 5.00 TO ARPM-VOID-YTD
           END-IF
           IF ARPM-VOID-YTD > 75.00
               MOVE 75.00 TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-STOP.
           IF ARPM-STOP-AMT < 100.00
               MOVE 100.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 5000.00
               MOVE 5000.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3210-FLAG-VOID.
           IF ARPM-VOID-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-AMT ROUNDED =
                   ARPT-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-AMT
           END-IF
           ADD ARPM-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-CHANNEL.
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
      * STOP YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3240-FLAG-STOP.
           IF ARPM-STOP-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-STOP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-FEE.
           IF ARPM-FEE-YTD < 25.00
               MOVE 25.00 TO ARPM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-FEE-YTD > 5000.00
               MOVE 5000.00 TO ARPM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIERX TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIERX-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPT-FEE-AMT NOT < WS-FEE-TIERX-LOW(WS-TX)
                   MOVE WS-FEE-TIERX-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E594 MEMO REQUIRED
      * E521 VOID YTD NEGATIVE
      * E610 VOID YTD OVER 10000
      * E869 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-MEMO = SPACES
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN ARPM-VOID-YTD < 0
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN ARPM-VOID-YTD > 10000.00
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARPM-FEE-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD ARPM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN LAST MAINT DATE IS PAST
      * MB -> MH WHEN LAST MAINT DATE IS PAST
      * MH -> UG WHEN LAST MAINT DATE IS PAST
      * UG -> UR WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-SOURCE.
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
      * SELECT THE FEE TIER TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF ARPT-ISSUED-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
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
       3320-SET-CHANNEL.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E711 OPERATOR REQUIRED
      * E659 OUTSTAND AMT NEGATIVE
      * E834 OUTSTAND AMT OVER 5000
      * E146 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-OPERATOR = SPACES
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT < 0
                   MOVE 'E659' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT > 5000.00
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
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
      * STOP YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3360-FLAG-STOP.
           IF ARPM-STOP-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-STOP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E092 MEMO REQUIRED
      * E504 VOID AMT NEGATIVE
      * E044 VOID AMT OVER 5000
      * E923 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3370-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-MEMO = SPACES
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN ARPT-VOID-AMT < 0
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN ARPT-VOID-AMT > 5000.00
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN ARPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E923' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 MEMO REQUIRED
      * E964 VOID AMT NEGATIVE
      * E980 VOID AMT OVER 1000000
      * E815 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-MEMO = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN ARPM-VOID-AMT < 0
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN ARPM-VOID-AMT > 1000000.00
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN ARPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3390-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPT-OUTSTAND-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPT-OUTSTAND-AMT - (WS-WORK-AMT *
               12)
           COMPUTE ARPM-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE PAID AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3430-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ARPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ARPM-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-PAID-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 10.00 TO ARPM-OUTSTAND-YTD
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 20.00 TO ARPM-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-PAID-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-OUTSTAND-YTD > 0 AND ARPM-OUTSTAND-YTD < 10.00
               MOVE 10.00 TO ARPM-OUTSTAND-YTD
           END-IF
           IF ARPM-OUTSTAND-YTD > 100.00
               MOVE 100.00 TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN LAST ACT DATE IS PAST
      * MB -> MH WHEN LAST ACT DATE IS PAST
      * MH -> UG WHEN LAST ACT DATE IS PAST
      * UG -> UR WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-SOURCE.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E578 MEMO REQUIRED
      * E056 VOID YTD NEGATIVE
      * E655 VOID YTD OVER 25000
      * E082 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-MEMO = SPACES
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN ARPM-VOID-YTD < 0
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN ARPM-VOID-YTD > 25000.00
                   MOVE 'E655' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN EFFECTIVE DATE IS PAST
      * MB -> MH WHEN EFFECTIVE DATE IS PAST
      * MH -> UG WHEN EFFECTIVE DATE IS PAST
      * UG -> UR WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-VOID.
           IF ARPM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-YTD * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VOID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3500-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-YTD ROUNDED =
                   ARPM-VOID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-PAID-YTD
           END-IF
           ADD ARPM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE STOP AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3510-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ARPT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPT-VOID-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO ARPT-VOID-AMT
               WHEN ARPT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO ARPT-VOID-AMT
               WHEN OTHER
                   COMPUTE ARPT-VOID-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPT-VOID-AMT > 0 AND ARPT-VOID-AMT < 1.00
               MOVE 1.00 TO ARPT-VOID-AMT
           END-IF
           IF ARPT-VOID-AMT > 150.00
               MOVE 150.00 TO ARPT-VOID-AMT
           END-IF
           ADD ARPT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-STOP.
           IF ARPM-STOP-AMT < 25.00
               MOVE 25.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 50000.00
               MOVE 50000.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3550-FLAG-OUTSTAND.
           IF ARPM-OUTSTAND-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-OUTSTAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE ARPT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE ARPT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN OPEN DATE IS PAST
      * PD -> SP WHEN OPEN DATE IS PAST
      * SP -> VD WHEN OPEN DATE IS PAST
      * VD -> FE WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-CYCLE.
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
      * SPLIT ISSUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E429 NAME REQUIRED
      * E587 ISSUED YTD NEGATIVE
      * E510 ISSUED YTD OVER 25000
      * E921 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-NAME = SPACES
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD < 0
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD > 25000.00
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E472 CUST ID REQUIRED
      * E398 PAID YTD NEGATIVE
      * E494 PAID YTD OVER 50000
      * E323 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN ARPM-PAID-YTD < 0
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN ARPM-PAID-YTD > 50000.00
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-AMT ROUNDED =
                   ARPM-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-OUTSTAND-AMT ROUNDED =
                   ARPM-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-OUTSTAND-AMT
           END-IF
           ADD ARPT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE STOP FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3690-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-STOP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPM-STOP-AMT ROUNDED = ARPT-OUTSTAND-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPT-TRAN-CD = 'IS'
                   MOVE 5.00 TO ARPM-STOP-AMT
               WHEN ARPT-TRAN-CD = 'PD'
                   MOVE 5.00 TO ARPM-STOP-AMT
               WHEN OTHER
                   COMPUTE ARPM-STOP-AMT ROUNDED = ARPT-OUTSTAND-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-STOP-AMT > 0 AND ARPM-STOP-AMT < 5.00
               MOVE 5.00 TO ARPM-STOP-AMT
           END-IF
           IF ARPM-STOP-AMT > 100.00
               MOVE 100.00 TO ARPM-STOP-AMT
           END-IF
           ADD ARPM-STOP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARPM-PAID-YTD NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3740-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-PAID-YTD
           END-IF
           ADD ARPM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-LAST-PAY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3770-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPT-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ARPT-OUTSTAND-AMT ROUNDED = ARPM-OUTSTAND-YTD
                       * ARPM-PENALTY-PCT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 12.00 TO ARPT-OUTSTAND-AMT
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 3.00 TO ARPT-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE ARPT-OUTSTAND-AMT ROUNDED = ARPM-OUTSTAND-YTD
                       * ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPT-OUTSTAND-AMT > 0 AND ARPT-OUTSTAND-AMT < 5.00
               MOVE 5.00 TO ARPT-OUTSTAND-AMT
           END-IF
           IF ARPT-OUTSTAND-AMT > 35.00
               MOVE 35.00 TO ARPT-OUTSTAND-AMT
           END-IF
           ADD ARPT-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3780-FLAG-FEE.
           IF ARPM-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STOP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-OUTSTAND-AMT ROUNDED =
                   ARPM-STOP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-OUTSTAND-AMT
           END-IF
           ADD ARPT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3800-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPM-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-STOP-FLAG = 'Y'
                   COMPUTE ARPM-ISSUED-YTD ROUNDED = ARPT-ISSUED-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 5.00 TO ARPM-ISSUED-YTD
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 3.00 TO ARPM-ISSUED-YTD
               WHEN OTHER
                   COMPUTE ARPM-ISSUED-YTD ROUNDED = ARPT-ISSUED-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-ISSUED-YTD > 0 AND ARPM-ISSUED-YTD < 10.00
               MOVE 10.00 TO ARPM-ISSUED-YTD
           END-IF
           IF ARPM-ISSUED-YTD > 150.00
               MOVE 150.00 TO ARPM-ISSUED-YTD
           END-IF
           ADD ARPM-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-EFFECTIVE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN NEXT DUE DATE IS PAST
      * SW -> UA WHEN NEXT DUE DATE IS PAST
      * UA -> WB WHEN NEXT DUE DATE IS PAST
      * WB -> ME WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-CHANNEL.
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
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-NEXT-DUE.
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
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-NEXT-REVIEW.
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
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CYCLE-CD
                   ADD ARPM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-ISSUED.
           IF ARPM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-AMT * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE VOID AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3880-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPM-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-ISSUED-YTD ROUNDED = ARPT-VOID-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 12.00 TO ARPM-ISSUED-YTD
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 12.00 TO ARPM-ISSUED-YTD
               WHEN OTHER
                   COMPUTE ARPM-ISSUED-YTD ROUNDED = ARPT-VOID-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-ISSUED-YTD > 0 AND ARPM-ISSUED-YTD < 10.00
               MOVE 10.00 TO ARPM-ISSUED-YTD
           END-IF
           IF ARPM-ISSUED-YTD > 150.00
               MOVE 150.00 TO ARPM-ISSUED-YTD
           END-IF
           ADD ARPM-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-BRANCH-CD
                   ADD ARPM-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E524 CONTACT REQUIRED
      * E626 WORK AMT NEGATIVE
      * E368 WORK AMT OVER 25000
      * E973 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-VOID.
           IF ARPT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-VOID-AMT * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPT-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STOP YTD.
      *----------------------------------------------------------------*
       3930-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-ISSUED-AMT ROUNDED =
                   ARPT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPT-ISSUED-AMT
           END-IF
           ADD ARPT-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-ISSUED.
           IF ARPM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-ISSUED-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-STOP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CP -> MR WHEN EFFECTIVE DATE IS PAST
      * MR -> PY WHEN EFFECTIVE DATE IS PAST
      * PY -> SP WHEN EFFECTIVE DATE IS PAST
      * SP -> TP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO ARPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN LAST STMT DATE IS PAST
      * SW -> UA WHEN LAST STMT DATE IS PAST
      * UA -> WB WHEN LAST STMT DATE IS PAST
      * WB -> ME WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-CHANNEL.
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
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-VOID.
           IF ARPT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-VOID-AMT * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E932 ADDR REQUIRED
      * E901 FEE AMT NEGATIVE
      * E279 FEE AMT OVER 5000
      * E284 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT < 0
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT > 5000.00
                   MOVE 'E279' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ARPM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-VOID.
           IF ARPM-VOID-AMT < 10.00
               MOVE 10.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-VOID-AMT > 5000.00
               MOVE 5000.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4070-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ARPM-PAID-YTD ROUNDED = ARPM-OUTSTAND-YTD *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 3.00 TO ARPM-PAID-YTD
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 3.00 TO ARPM-PAID-YTD
               WHEN OTHER
                   COMPUTE ARPM-PAID-YTD ROUNDED = ARPM-OUTSTAND-YTD *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-PAID-YTD > 0 AND ARPM-PAID-YTD < 10.00
               MOVE 10.00 TO ARPM-PAID-YTD
           END-IF
           IF ARPM-PAID-YTD > 35.00
               MOVE 35.00 TO ARPM-PAID-YTD
           END-IF
           ADD ARPM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4090-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-ISSUED.
           IF ARPM-ISSUED-YTD < 50.00
               MOVE 50.00 TO ARPM-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-YTD > 5000.00
               MOVE 5000.00 TO ARPM-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-FEE.
           IF ARPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-FEE-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4130-FLAG-PAID.
           IF ARPM-PAID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST STMT DATE IS PAST
      * PD -> SP WHEN LAST STMT DATE IS PAST
      * SP -> VD WHEN LAST STMT DATE IS PAST
      * VD -> FE WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO ARPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARPT-ISSUED-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-NEXT-REVIEW.
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
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4180-LIMIT-STOP.
           IF ARPM-STOP-AMT < 0.00
               MOVE 0.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 2500.00
               MOVE 2500.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RECON TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-RECON-TYPE.
           EVALUATE ARPM-RECON-TYPE-CD
               WHEN 'EEH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KBS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PAC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VKT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YMX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZVT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-VOID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E678 OPERATOR REQUIRED
      * E983 STOP AMT NEGATIVE
      * E685 STOP AMT OVER 5000
      * E498 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4220-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-OPERATOR = SPACES
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN ARPM-STOP-AMT < 0
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN ARPM-STOP-AMT > 5000.00
                   MOVE 'E685' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E145 REJECT CD REQUIRED
      * E132 OUTSTAND YTD NEGATIVE
      * E736 OUTSTAND YTD OVER 50000
      * E123 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 50000.00
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E123' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4240-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E406 DESC REQUIRED
      * E896 OUTSTAND YTD NEGATIVE
      * E978 OUTSTAND YTD OVER 10000
      * E736 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 10000.00
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN ARPM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED YTD.
      *----------------------------------------------------------------*
       4260-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-ISSUED-YTD ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-ISSUED-YTD
           END-IF
           ADD ARPM-ISSUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E098 DESC REQUIRED
      * E451 FEE AMT NEGATIVE
      * E602 FEE AMT OVER 5000
      * E022 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT < 0
                   MOVE 'E451' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT > 5000.00
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN ARPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4280-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPT-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E564 CONTACT REQUIRED
      * E796 ISSUED AMT NEGATIVE
      * E775 ISSUED AMT OVER 25000
      * E858 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-AMT < 0
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-AMT > 25000.00
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN ARPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STOP YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4300-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-SOURCE-CD
                   ADD ARPM-STOP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-STOP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4310-LIMIT-OUTSTAND.
           IF ARPM-OUTSTAND-AMT < 100.00
               MOVE 100.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-OUTSTAND-AMT > 10000.00
               MOVE 10000.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4320-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4330-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4350-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-YTD ROUNDED =
                   ARPT-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE PAID YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4360-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ARPM-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 20.00 TO ARPM-OUTSTAND-YTD
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 10.00 TO ARPM-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-OUTSTAND-YTD > 0 AND ARPM-OUTSTAND-YTD < 2.00
               MOVE 2.00 TO ARPM-OUTSTAND-YTD
           END-IF
           IF ARPM-OUTSTAND-YTD > 75.00
               MOVE 75.00 TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4370-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPM-VOID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-VOID-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4380-CHECK-LAST-ACT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4390-FLAG-FEE.
           IF ARPM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4410-LIMIT-VOID.
           IF ARPT-VOID-AMT < 25.00
               MOVE 25.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 1000.00
               MOVE 1000.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4430-LIMIT-VOID.
           IF ARPT-VOID-AMT < 100.00
               MOVE 100.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 10000.00
               MOVE 10000.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4440-LIMIT-PAID.
           IF ARPM-PAID-YTD < 100.00
               MOVE 100.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-PAID-YTD > 2500.00
               MOVE 2500.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E706 CONTACT REQUIRED
      * E557 STOP AMT NEGATIVE
      * E446 STOP AMT OVER 5000
      * E376 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4450-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN ARPM-STOP-AMT < 0
                   MOVE 'E557' TO WS-REJECT-CODE
               WHEN ARPM-STOP-AMT > 5000.00
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4470-LIMIT-VOID.
           IF ARPT-VOID-AMT < 0.00
               MOVE 0.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 10000.00
               MOVE 10000.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4480-DESCRIBE-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
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
      * ACCUMULATE ISSUED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4490-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-CHANNEL-CD
                   ADD ARPM-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4500-FLAG-PAID.
           IF ARPM-PAID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4510-AGE-NEXT-DUE.
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
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       4520-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   ARPM-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VOID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4530-FLAG-VOID.
           IF ARPT-VOID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPT-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4540-ACCRUE-STOP.
           IF ARPM-STOP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-AMT * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4550-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4560-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4570-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE ARPM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4580-FLAG-OUTSTAND.
           IF ARPT-OUTSTAND-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPT-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       4590-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-AMT ROUNDED =
                   ARPM-OUTSTAND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE STOP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       4600-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-AMT ROUNDED =
                   ARPM-STOP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E234 REF NO REQUIRED
      * E602 FEE YTD NEGATIVE
      * E741 FEE YTD OVER 50000
      * E752 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4610-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-REF-NO = SPACES
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN ARPM-FEE-YTD < 0
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN ARPM-FEE-YTD > 50000.00
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN ARPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4620-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARPM-ISSUED-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4630-CHECK-LAST-ACT.
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
               MOVE 'Y' TO ARPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4640-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4650-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-YTD - (WS-WORK-AMT * 2)
           COMPUTE ARPM-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       4660-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-OUTSTAND-YTD ROUNDED =
                   ARPM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN NEXT DUE DATE IS PAST
      * MB -> MH WHEN NEXT DUE DATE IS PAST
      * MH -> UG WHEN NEXT DUE DATE IS PAST
      * UG -> UR WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4670-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIERX TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIERX-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ARPM-VOID-AMT NOT < WS-FEE-TIERX-LOW(WS-TX)
                   MOVE WS-FEE-TIERX-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4690-CHECK-CLOSE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

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
                   MOVE ARDTE-FEE-YTD
                     TO ARPM-FEE-YTD
                   MOVE ARDTE-PAID-YTD
                     TO ARPM-PAID-YTD
                   MOVE ARDTE-VOID-YTD
                     TO ARPM-VOID-YTD
                   MOVE ARDTE-FEE-AMT
                     TO ARPM-FEE-AMT
                   MOVE ARDTE-VOID-AMT
                     TO ARPM-VOID-AMT
                   MOVE ARDTE-PREFERRED-FLAG
                     TO ARPM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-ARDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-ARSTAT.
           INITIALIZE ARSTAT-PARMS
           MOVE ARPM-ISSUED-YTD
             TO ARSTAT-ISSUED-YTD
           MOVE ARPM-FEE-AMT
             TO ARSTAT-FEE-AMT
           MOVE ARPM-OUTSTAND-AMT
             TO ARSTAT-OUTSTAND-AMT
           MOVE ARPM-PAID-AMT
             TO ARSTAT-PAID-AMT
           MOVE ARPM-ISSUED-AMT
             TO ARSTAT-ISSUED-AMT
           MOVE ARPM-OUTSTAND-YTD
             TO ARSTAT-OUTSTAND-YTD
           MOVE ARPM-CLOSE-DATE
             TO ARSTAT-CLOSE-DATE
           MOVE ARPM-LAST-STMT-DATE
             TO ARSTAT-LAST-STMT-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARSTAT-NEXT-REVIEW-DATE
           MOVE ARPM-EFFECTIVE-DATE
             TO ARSTAT-EFFECTIVE-DATE
           MOVE ARPM-BRANCH-CD
             TO ARSTAT-BRANCH-CD
           MOVE ARPM-CHANNEL-CD
             TO ARSTAT-CHANNEL-CD
           MOVE ARPM-CYCLE-CD
             TO ARSTAT-CYCLE-CD
           MOVE ARPM-SOURCE-CD
             TO ARSTAT-SOURCE-CD
           MOVE ARPM-NOTICE-SENT-FLAG
             TO ARSTAT-NOTICE-SENT-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARSTAT-PREFERRED-FLAG
           MOVE ARPM-OVERRIDE-FLAG
             TO ARSTAT-OVERRIDE-FLAG
           MOVE ARPM-ITEM-CNT
             TO ARSTAT-ITEM-CNT
           MOVE ARPM-CYCLE-CNT
             TO ARSTAT-CYCLE-CNT
           MOVE ARPM-PENALTY-PCT
             TO ARSTAT-PENALTY-PCT
           MOVE ARPM-DISCOUNT
             TO ARSTAT-DISCOUNT
           MOVE ARPM-CUST-ID
             TO ARSTAT-CUST-ID
           MOVE ARPM-REF-NO
             TO ARSTAT-REF-NO
           CALL 'ARSTAT' USING ARSTAT-PARMS
           EVALUATE ARSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARSTAT-FEE-AMT
                     TO ARPM-FEE-AMT
                   MOVE ARSTAT-OUTSTAND-AMT
                     TO ARPM-OUTSTAND-AMT
                   MOVE ARSTAT-NOTICE-SENT-FLAG
                     TO ARPM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-ARSTAT' TO ABND-PARAGRAPH
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
                   MOVE ARPRC-HOLD-FLAG
                     TO ARPM-HOLD-FLAG
                   MOVE ARPRC-STOP-FLAG
                     TO ARPM-STOP-FLAG
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
           DISPLAY 'ARPXMIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ARPXMIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
