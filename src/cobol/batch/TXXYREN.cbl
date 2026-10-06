      *================================================================*
      * PROGRAM  : TXXYREN                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELEX WIRE MESSAGING - YEAR-END. READS THE TLX      *
      *            TRANSACTIONS, UPDATES THE TLX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TESTED TELEX FUNDS TRANSFER     *
      *            MESSAGES AND TEST KEY VALIDATION                    *
      * INPUT    : TLXMST (KSDS), TLXTRN, BANKCTL                      *
      * OUTPUT   : TLXOUT, TXXYRRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TXPRC, TXELG            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-10-02 GAF  ORIGINAL                                       *
      * 2000-04-16 JLM  QUEUED RERUN PROTECTION PRB6880                *
      * 2003-09-03 JLM  SETTLE STATUS CODES ADDED REQ8238              *
      * 2006-07-15 JLM  CORRESP ROUNDING - HALF UP REQ6668             *
      * 2007-11-21 JLM  CHARGE FEE SCHEDULE UPDATE CHG5598             *
      * 2009-11-10 SKN  SETTLE CALCULATION CORRECTED CHG4533           *
      * 2009-12-19 DPS  CORRESP RERUN PROTECTION PRB8292               *
      * 2010-03-21 PQR  PERFORMANCE - WIRE READ AHEAD REQ9687          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXXYREN.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TLXMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TLXM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TLXTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TLXOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TXXYRRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TLXMST.
       FD  TRAN-FILE.
       COPY TLXTRN.
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
      * WS-QUEUED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-QUEUED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000576000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000882000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000623000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000756000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000047000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000620500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006400000000487000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006450000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000207000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000780000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000194000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000803500'.
       01  WS-QUEUED-TIER-TBL REDEFINES WS-QUEUED-TIER-VALUES.
           05  WS-QUEUED-TIER-ROW OCCURS 12 TIMES.
               10  WS-QUEUED-TIER-LOW      PIC 9(09)V99.
               10  WS-QUEUED-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-QUEUED-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-QUEUED-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000777000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000828500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000123500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000295000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000305000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000700000000061000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000734000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000769000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000343000'.
       01  WS-QUEUED-TIERX-TBL REDEFINES WS-QUEUED-TIERX-VALUES.
           05  WS-QUEUED-TIERX-ROW OCCURS 9 TIMES.
               10  WS-QUEUED-TIERX-LOW     PIC 9(09)V99.
               10  WS-QUEUED-TIERX-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CHARGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CHARGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000698500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000051000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000631500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000529000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000420000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000169000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011850000000782000'.
       01  WS-CHARGE-SCHED-TBL REDEFINES WS-CHARGE-SCHED-VALUES.
           05  WS-CHARGE-SCHED-ROW OCCURS 7 TIMES.
               10  WS-CHARGE-SCHED-LOW     PIC 9(09)V99.
               10  WS-CHARGE-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CORRESP-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CORRESP-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000028500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000697500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000523000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000431000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000611000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000543500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000374000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000715000'.
       01  WS-CORRESP-BAND-TBL REDEFINES WS-CORRESP-BAND-VALUES.
           05  WS-CORRESP-BAND-ROW OCCURS 8 TIMES.
               10  WS-CORRESP-BAND-LOW     PIC 9(09)V99.
               10  WS-CORRESP-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TXXYREN  TELEX WIRE MESSAGING'.
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
       COPY TXPRCP.
       COPY TXELGP.

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
               MOVE 'TLXMST' TO ABND-FILE-NAME
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
           MOVE TLXT-KEY TO TLXM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-EDIT-MEMO
               PERFORM 3010-AGE-LAST-ACT
               PERFORM 3020-SET-TYPE
               PERFORM 3030-COUNT-PAYMENT
               PERFORM 3040-EDIT-NAME
               PERFORM 3050-DESCRIBE-MSG-TYPE
               PERFORM 3060-SET-BRANCH
               PERFORM 3070-DESCRIBE-BRANCH
               PERFORM 3080-COUNT-SEQ
               PERFORM 3090-AGE-NEXT-DUE
               PERFORM 3100-ACCUM-TEST-KEY
               PERFORM 3110-AGE-EFF
               PERFORM 3120-AGE-EXPIRY
               PERFORM 3130-DESCRIBE-MSG-TYPE
               PERFORM 3140-FLAG-ITEM
               PERFORM 3150-ASSESS-NOTICE-SENT
               PERFORM 3160-DESCRIBE-TYPE
               PERFORM 3170-SELECT-DISCOUNT
               PERFORM 3180-SELECT-DISCOUNT
               PERFORM 3190-ACCRUE-FEE-PCT
               PERFORM 3200-EDIT-MEMO
               PERFORM 3210-FLAG-QUEUED
               PERFORM 3220-ACCRUE-CHARGE
               PERFORM 3230-ACCRUE-WIRE
               PERFORM 3240-SPLIT-SETTLE
               PERFORM 3250-SPLIT-SETTLE
               PERFORM 3260-EDIT-MEMO
               PERFORM 3270-ACCUM-CHANNEL
               PERFORM 3280-CHECK-EFF
               PERFORM 3290-CHECK-EXPIRY
               PERFORM 3300-ACCRUE-SETTLE
               PERFORM 3310-LIMIT-COVER
               PERFORM 3320-SELECT-FEE-PCT
               PERFORM 3330-ACCUM-REASON
               PERFORM 3340-EDIT-MEMO
               PERFORM 3350-DESCRIBE-CHANNEL
               PERFORM 3360-SPLIT-SETTLE
               PERFORM 3370-FLAG-CHARGE
               PERFORM 3380-ACCUM-TEST-KEY
               PERFORM 3390-ASSESS-OVERRIDE
               PERFORM 3400-COUNT-PAYMENT
               PERFORM 3410-COUNT-EXCEPT
               PERFORM 3420-ACCRUE-QUEUED
               PERFORM 3430-FLAG-COVER
               PERFORM 3440-DESCRIBE-MSG-TYPE
               PERFORM 3450-PRORATE-COVER
               PERFORM 3460-AGE-NOTICE
               PERFORM 3470-CHECK-EXPIRY
               PERFORM 3480-SET-REGION
               PERFORM 3490-EDIT-OPERATOR
               PERFORM 3500-CHECK-NOTICE
               PERFORM 3510-EDIT-ADDR
               PERFORM 3520-ACCRUE-WIRE
               PERFORM 3530-DESCRIBE-REASON
               PERFORM 3540-ACCRUE-COVER
               PERFORM 3550-PRORATE-COVER
               PERFORM 3560-LIMIT-COVER
               PERFORM 3570-COUNT-NOTICE
               PERFORM 3580-COUNT-EXCEPT
               PERFORM 3590-PRORATE-SETTLE
               PERFORM 3600-ACCUM-BRANCH
               PERFORM 3610-ASSESS-PREFERRED
               PERFORM 3620-CHECK-NEXT-DUE
               PERFORM 3630-COUNT-ITEM
               PERFORM 3640-AGE-LAST-PAY
               PERFORM 3650-SPLIT-CHARGE
               PERFORM 3660-PRORATE-CHARGE
               PERFORM 3670-ACCUM-TYPE
               PERFORM 3680-EDIT-CONTACT
               PERFORM 3690-FLAG-SETTLE
               PERFORM 3700-ACCUM-TEST-KEY
               PERFORM 3710-SELECT-FEE-PCT
               PERFORM 3720-ACCUM-REASON
               PERFORM 3730-EDIT-DESC
               PERFORM 3740-ACCUM-TRAN
               PERFORM 3750-SET-CHANNEL
               PERFORM 3760-CHECK-LAST-ACT
               PERFORM 3770-CHECK-LAST-PAY
               PERFORM 3780-LIMIT-WIRE
               PERFORM 3790-ACCRUE-QUEUED
               PERFORM 3800-ACCUM-TRAN
               PERFORM 3810-COUNT-SEQ
               PERFORM 3820-AGE-EFFECTIVE
               PERFORM 3830-ACCUM-CHANNEL
               PERFORM 3840-DESCRIBE-BRANCH
               PERFORM 3850-EDIT-CONTACT
               PERFORM 3860-LIMIT-WIRE
               PERFORM 3870-FLAG-CORRESP
               PERFORM 3880-SET-TEST-KEY
               PERFORM 3890-LIMIT-COVER
               PERFORM 3900-ASSESS-NOTICE-SENT
               PERFORM 3910-EDIT-NAME
               PERFORM 3920-PRORATE-CORRESP
               PERFORM 3930-ACCUM-REASON
               PERFORM 3940-SPLIT-CORRESP
               PERFORM 3950-FLAG-ITEM
               PERFORM 3960-FLAG-COVER
               PERFORM 3970-ASSESS-PREFERRED
               PERFORM 3980-EDIT-OPERATOR
               PERFORM 3990-DESCRIBE-TEST-KEY
               PERFORM 4000-ACCRUE-CHARGE
               PERFORM 4010-ACCUM-TEST-KEY
               PERFORM 4020-PRORATE-QUEUED
               PERFORM 4030-SPLIT-CHARGE
               PERFORM 4040-COUNT-NOTICE
               PERFORM 4050-LIMIT-CHARGE
               PERFORM 4060-SET-CHANNEL
               PERFORM 4070-SPLIT-CHARGE
               PERFORM 4080-ACCRUE-QUEUED
               PERFORM 4090-CHECK-LAST-PAY
               PERFORM 4100-EDIT-ADDR
               PERFORM 4110-EDIT-OPERATOR
               PERFORM 4120-AGE-OPEN
               PERFORM 4130-ACCUM-CHANNEL
               PERFORM 4140-SELECT-RATE
               PERFORM 4150-SELECT-RATE
               PERFORM 4160-ASSESS-REVERSAL
               PERFORM 4170-SET-TRAN
               PERFORM 4180-CHECK-OPEN
               PERFORM 4190-FLAG-ITEM
               PERFORM 4200-EDIT-OPERATOR
               PERFORM 4210-CHECK-EFF
               PERFORM 4220-ASSESS-SUSPEND
               PERFORM 4230-AGE-POST
               PERFORM 4240-SELECT-DISCOUNT
               PERFORM 4250-EDIT-REF-NO
               PERFORM 4260-SELECT-DISCOUNT
               PERFORM 4270-FLAG-COVER
               PERFORM 4280-SPLIT-QUEUED
               PERFORM 4290-AGE-EXPIRY
               PERFORM 4300-LIMIT-QUEUED
               PERFORM 4310-ACCUM-MSG-TYPE
               PERFORM 4320-FLAG-QUEUED
               PERFORM 4330-AGE-LAST-PAY
               PERFORM 4340-DESCRIBE-MSG-TYPE
               PERFORM 4350-PRORATE-CHARGE
               PERFORM 4360-SELECT-FEE-PCT
               PERFORM 4370-FLAG-SETTLE
               PERFORM 4380-DESCRIBE-REGION
               PERFORM 4390-SET-CHANNEL
               PERFORM 4400-EDIT-REF-NO
               PERFORM 4410-SPLIT-CHARGE
               PERFORM 4420-LIMIT-WIRE
               PERFORM 4430-ACCRUE-WIRE
               PERFORM 4440-ACCUM-REASON
               PERFORM 4450-ACCRUE-SETTLE
               PERFORM 4460-COUNT-NOTICE
               PERFORM 4470-EDIT-MEMO
               PERFORM 4480-PRORATE-SETTLE
               PERFORM 7020-CALL-TXPRC
               PERFORM 7000-CALL-TXELG
               REWRITE TLX-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TLXMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TLX-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E087 MEMO REQUIRED
      * E699 WORK AMT NEGATIVE
      * E934 WORK AMT OVER 250000
      * E626 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN TLXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> P WHEN NEXT REVIEW DATE IS PAST
      * P -> W WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E428 NAME REQUIRED
      * E355 CHARGE YTD NEGATIVE
      * E649 CHARGE YTD OVER 250000
      * E022 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-NAME = SPACES
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD < 0
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD > 250000.00
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN TLXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN CLOSE DATE IS PAST
      * KR -> LD WHEN CLOSE DATE IS PAST
      * LD -> LS WHEN CLOSE DATE IS PAST
      * LS -> XL WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 12
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED YTD BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TEST-KEY-CD
                   ADD TLXM-QUEUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-QUEUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3140-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COVER YTD FEE ON THE QUEUED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3150-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXM-COVER-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXM-COVER-YTD ROUNDED = TLXM-QUEUED-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO TLXM-COVER-YTD
               WHEN TLXT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO TLXM-COVER-YTD
               WHEN OTHER
                   COMPUTE TLXM-COVER-YTD ROUNDED = TLXM-QUEUED-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-COVER-YTD > 0 AND TLXM-COVER-YTD < 5.00
               MOVE 5.00 TO TLXM-COVER-YTD
           END-IF
           IF TLXM-COVER-YTD > 25.00
               MOVE 25.00 TO TLXM-COVER-YTD
           END-IF
           ADD TLXM-COVER-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3160-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED TIER TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-TIER-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TLXM-CHARGE-AMT NOT < WS-QUEUED-TIER-LOW(WS-TX)
                   MOVE WS-QUEUED-TIER-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP BAND TIER FOR THE COVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-BAND-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-COVER-YTD NOT < WS-CORRESP-BAND-LOW(WS-TX)
                   MOVE WS-CORRESP-BAND-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E468 MEMO REQUIRED
      * E833 CORRESP AMT NEGATIVE
      * E272 CORRESP AMT OVER 1000000
      * E803 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN TLXM-CORRESP-AMT < 0
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN TLXM-CORRESP-AMT > 1000000.00
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN TLXT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E803' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3210-FLAG-QUEUED.
           IF TLXT-QUEUED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXT-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-CHARGE.
           IF TLXM-CHARGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-AMT * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-WIRE.
           IF TLXM-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-AMT * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CHARGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXT-SETTLE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXT-SETTLE-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXT-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E722 MEMO REQUIRED
      * E357 QUEUED AMT NEGATIVE
      * E564 QUEUED AMT OVER 25000
      * E902 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT < 0
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT > 25000.00
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD TLXM-WIRE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-WIRE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3280-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-SETTLE.
           IF TLXM-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-AMT * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-QUEUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COVER AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3310-LIMIT-COVER.
           IF TLXT-COVER-AMT < 100.00
               MOVE 100.00 TO TLXT-COVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-COVER-AMT > 5000.00
               MOVE 5000.00 TO TLXT-COVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED TIER TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TLXM-QUEUED-AMT NOT < WS-QUEUED-TIER-LOW(WS-TX)
                   MOVE WS-QUEUED-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REASON-CD
                   ADD TLXM-WIRE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-WIRE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E518 MEMO REQUIRED
      * E372 COVER YTD NEGATIVE
      * E896 COVER YTD OVER 5000
      * E829 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD < 0
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD > 5000.00
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN TLXM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E829' TO WS-REJECT-CODE
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
       3350-DESCRIBE-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE TLXM-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3370-FLAG-CHARGE.
           IF TLXM-CHARGE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED AMT BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TEST-KEY-CD
                   ADD TLXT-QUEUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-QUEUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE QUEUED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3390-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLXM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-MSG-TYPE-CD = 'LG'
                   MOVE 3.00 TO TLXM-SETTLE-AMT
               WHEN TLXM-MSG-TYPE-CD = 'LX'
                   MOVE 3.00 TO TLXM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-SETTLE-AMT > 0 AND TLXM-SETTLE-AMT < 2.00
               MOVE 2.00 TO TLXM-SETTLE-AMT
           END-IF
           IF TLXM-SETTLE-AMT > 25.00
               MOVE 25.00 TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3420-ACCRUE-QUEUED.
           IF TLXM-QUEUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-QUEUED-YTD * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXT-WIRE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COVER YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-COVER.
           IF TLXM-COVER-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-COVER-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHARGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-COVER-AMT ROUNDED =
                   TLXM-CHARGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-COVER-AMT
           END-IF
           ADD TLXM-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * G -> M WHEN POST DATE IS PAST
      * M -> T WHEN POST DATE IS PAST
      * T -> G WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-REGION.
           EVALUATE TLXM-REGION-CD
               WHEN 'G'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E241 OPERATOR REQUIRED
      * E488 SETTLE AMT NEGATIVE
      * E981 SETTLE AMT OVER 5000
      * E861 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT < 0
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT > 5000.00
                   MOVE 'E981' TO WS-REJECT-CODE
               WHEN TLXM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E861' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E076 ADDR REQUIRED
      * E043 QUEUED AMT NEGATIVE
      * E644 QUEUED AMT OVER 5000
      * E284 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-ADDR = SPACES
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT < 0
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT > 5000.00
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN TLXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-WIRE.
           IF TLXT-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-WIRE-AMT * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-COVER.
           IF TLXM-COVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-COVER-AMT * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXT-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3550-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TLXM-COVER-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COVER YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3560-LIMIT-COVER.
           IF TLXM-COVER-YTD < 100.00
               MOVE 100.00 TO TLXM-COVER-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-COVER-YTD > 2500.00
               MOVE 2500.00 TO TLXM-COVER-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE YTD.
      *----------------------------------------------------------------*
       3590-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-YTD ROUNDED =
                   TLXM-CORRESP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-SETTLE-YTD
           END-IF
           ADD TLXM-SETTLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-BRANCH-CD
                   ADD TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3610-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TLXM-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE TLXM-QUEUED-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-REGION-CD = 'G'
                   MOVE 20.00 TO TLXM-QUEUED-AMT
               WHEN TLXM-REGION-CD = 'M'
                   MOVE 3.00 TO TLXM-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXM-QUEUED-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-QUEUED-AMT > 0 AND TLXM-QUEUED-AMT < 10.00
               MOVE 10.00 TO TLXM-QUEUED-AMT
           END-IF
           IF TLXM-QUEUED-AMT > 50.00
               MOVE 50.00 TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD TLXT-COVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-COVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E872 CONTACT REQUIRED
      * E551 WIRE AMT NEGATIVE
      * E938 WIRE AMT OVER 99999
      * E878 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT < 0
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT > 99999.00
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN TLXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3690-FLAG-SETTLE.
           IF TLXT-SETTLE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXT-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE AMT BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TEST-KEY-CD
                   ADD TLXT-WIRE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-WIRE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED TIER TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TLXM-SETTLE-YTD NOT < WS-QUEUED-TIER-LOW(WS-TX)
                   MOVE WS-QUEUED-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REASON-CD
                   ADD TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E016 DESC REQUIRED
      * E518 WIRE YTD NEGATIVE
      * E874 WIRE YTD OVER 250000
      * E700 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-YTD < 0
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-YTD > 250000.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN TLXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-TRAN-CD
                   ADD TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-WIRE.
           IF TLXM-WIRE-YTD < 25.00
               MOVE 25.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-YTD > 5000.00
               MOVE 5000.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-QUEUED.
           IF TLXT-QUEUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-QUEUED-AMT * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 3
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E681 CONTACT REQUIRED
      * E530 COVER YTD NEGATIVE
      * E037 COVER YTD OVER 250000
      * E130 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E681' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD < 0
                   MOVE 'E530' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD > 250000.00
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN TLXT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-WIRE.
           IF TLXT-WIRE-AMT < 10.00
               MOVE 10.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-WIRE-AMT > 10000.00
               MOVE 10000.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-CORRESP.
           IF TLXM-CORRESP-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXM-CORRESP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TEST KEY CD:
      * EA -> GA WHEN NEXT REVIEW DATE IS PAST
      * GA -> HL WHEN NEXT REVIEW DATE IS PAST
      * HL -> JC WHEN NEXT REVIEW DATE IS PAST
      * JC -> KC WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GA' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GA'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HL' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COVER AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-COVER.
           IF TLXM-COVER-AMT < 100.00
               MOVE 100.00 TO TLXM-COVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-COVER-AMT > 1000.00
               MOVE 1000.00 TO TLXM-COVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE SETTLE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3900-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-SETTLE-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN TLXT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-SETTLE-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E435 NAME REQUIRED
      * E617 CHARGE YTD NEGATIVE
      * E362 CHARGE YTD OVER 1000000
      * E146 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-NAME = SPACES
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD < 0
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD > 1000000.00
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CORRESP AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-AMT ROUNDED =
                   TLXM-QUEUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REASON-CD
                   ADD TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-QUEUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3950-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COVER AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3960-FLAG-COVER.
           IF TLXT-COVER-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXT-COVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP YTD FEE ON THE COVER YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3970-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CORRESP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE TLXM-CORRESP-YTD ROUNDED = TLXM-COVER-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 12.00 TO TLXM-CORRESP-YTD
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 5.00 TO TLXM-CORRESP-YTD
               WHEN OTHER
                   COMPUTE TLXM-CORRESP-YTD ROUNDED = TLXM-COVER-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-CORRESP-YTD > 0 AND TLXM-CORRESP-YTD < 1.00
               MOVE 1.00 TO TLXM-CORRESP-YTD
           END-IF
           IF TLXM-CORRESP-YTD > 100.00
               MOVE 100.00 TO TLXM-CORRESP-YTD
           END-IF
           ADD TLXM-CORRESP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E759 OPERATOR REQUIRED
      * E273 SETTLE AMT NEGATIVE
      * E033 SETTLE AMT OVER 1000000
      * E420 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN TLXT-SETTLE-AMT < 0
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN TLXT-SETTLE-AMT > 1000000.00
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN TLXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TEST KEY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-CHARGE.
           IF TLXM-CHARGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED AMT BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TEST-KEY-CD
                   ADD TLXM-QUEUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-QUEUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED YTD.
      *----------------------------------------------------------------*
       4020-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-YTD ROUNDED =
                   TLXT-WIRE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-QUEUED-YTD
           END-IF
           ADD TLXM-QUEUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-YTD - (WS-WORK-AMT * 12)
           COMPUTE TLXM-COVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 0.00
               MOVE 0.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 1000.00
               MOVE 1000.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-QUEUED.
           IF TLXM-QUEUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-QUEUED-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E052 ADDR REQUIRED
      * E846 QUEUED YTD NEGATIVE
      * E297 QUEUED YTD OVER 99999
      * E972 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-ADDR = SPACES
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-YTD < 0
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-YTD > 99999.00
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E459 OPERATOR REQUIRED
      * E700 WIRE AMT NEGATIVE
      * E181 WIRE AMT OVER 1000000
      * E982 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT < 0
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT > 1000000.00
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD TLXM-QUEUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-QUEUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP BAND TIER FOR THE CHARGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-CHARGE-YTD NOT < WS-CORRESP-BAND-LOW(WS-TX)
                   MOVE WS-CORRESP-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED TIER TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-TIER-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TLXM-QUEUED-YTD NOT < WS-QUEUED-TIER-LOW(WS-TX)
                   MOVE WS-QUEUED-TIER-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE FEE ON THE WIRE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4160-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TLXT-WIRE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE TLXT-WIRE-AMT ROUNDED = TLXM-WIRE-YTD *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 12.00 TO TLXT-WIRE-AMT
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 15.00 TO TLXT-WIRE-AMT
               WHEN OTHER
                   COMPUTE TLXT-WIRE-AMT ROUNDED = TLXM-WIRE-YTD *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-WIRE-AMT > 0 AND TLXT-WIRE-AMT < 10.00
               MOVE 10.00 TO TLXT-WIRE-AMT
           END-IF
           IF TLXT-WIRE-AMT > 25.00
               MOVE 25.00 TO TLXT-WIRE-AMT
           END-IF
           ADD TLXT-WIRE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> OT WHEN NEXT DUE DATE IS PAST
      * OT -> RJ WHEN NEXT DUE DATE IS PAST
      * RJ -> RP WHEN NEXT DUE DATE IS PAST
      * RP -> CN WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OT' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'OT'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RP' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RP'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4190-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E895 OPERATOR REQUIRED
      * E292 WIRE AMT NEGATIVE
      * E403 WIRE AMT OVER 5000
      * E082 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E895' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT < 0
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT > 5000.00
                   MOVE 'E403' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4220-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLXT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE TLXT-SETTLE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 3.00 TO TLXT-SETTLE-AMT
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 12.00 TO TLXT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXT-SETTLE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXT-SETTLE-AMT > 0 AND TLXT-SETTLE-AMT < 5.00
               MOVE 5.00 TO TLXT-SETTLE-AMT
           END-IF
           IF TLXT-SETTLE-AMT > 250.00
               MOVE 250.00 TO TLXT-SETTLE-AMT
           END-IF
           ADD TLXT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP BAND TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-BAND-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-SETTLE-YTD NOT < WS-CORRESP-BAND-LOW(WS-TX)
                   MOVE WS-CORRESP-BAND-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E258 REF NO REQUIRED
      * E802 WORK AMT NEGATIVE
      * E519 WORK AMT OVER 99999
      * E279 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-REF-NO = SPACES
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN TLXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E279' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED TIERX TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-TIERX-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-QUEUED-YTD NOT < WS-QUEUED-TIERX-LOW(WS-TX)
                   MOVE WS-QUEUED-TIERX-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COVER YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4270-FLAG-COVER.
           IF TLXM-COVER-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-COVER-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXM-QUEUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-QUEUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE TLXM-COVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4300-LIMIT-QUEUED.
           IF TLXM-QUEUED-YTD < 10.00
               MOVE 10.00 TO TLXM-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-QUEUED-YTD > 1000.00
               MOVE 1000.00 TO TLXM-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP YTD BY MSG TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-MSG-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-MSG-TYPE-CD
                   ADD TLXM-CORRESP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-MSG-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4320-FLAG-QUEUED.
           IF TLXT-QUEUED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXT-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4330-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ND'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE QUEUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHARGE AMT.
      *----------------------------------------------------------------*
       4350-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CHARGE-AMT ROUNDED =
                   TLXT-QUEUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-CHARGE-AMT
           END-IF
           ADD TLXM-CHARGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED TIER TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4360-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TLXM-QUEUED-AMT NOT < WS-QUEUED-TIER-LOW(WS-TX)
                   MOVE WS-QUEUED-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4370-FLAG-SETTLE.
           IF TLXM-SETTLE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4380-DESCRIBE-REGION.
           EVALUATE TLXM-REGION-CD
               WHEN 'G'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4390-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E941 REF NO REQUIRED
      * E409 WORK AMT NEGATIVE
      * E650 WORK AMT OVER 1000000
      * E856 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-REF-NO = SPACES
                   MOVE 'E941' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN TLXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4420-LIMIT-WIRE.
           IF TLXM-WIRE-AMT < 0.00
               MOVE 0.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-AMT > 50000.00
               MOVE 50000.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4430-ACCRUE-WIRE.
           IF TLXM-WIRE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-YTD * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXM-QUEUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4450-ACCRUE-SETTLE.
           IF TLXT-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-SETTLE-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXT-WIRE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4460-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E595 MEMO REQUIRED
      * E124 COVER AMT NEGATIVE
      * E085 COVER AMT OVER 250000
      * E621 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4470-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN TLXM-COVER-AMT < 0
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN TLXM-COVER-AMT > 250000.00
                   MOVE 'E085' TO WS-REJECT-CODE
               WHEN TLXM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       4480-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-AMT ROUNDED =
                   TLXM-SETTLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TXPRC.
           INITIALIZE TXPRC-PARMS
           MOVE TLXM-COVER-YTD
             TO TXPRC-COVER-YTD
           MOVE TLXM-QUEUED-AMT
             TO TXPRC-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXPRC-CORRESP-YTD
           MOVE TLXM-SETTLE-YTD
             TO TXPRC-SETTLE-YTD
           MOVE TLXM-QUEUED-YTD
             TO TXPRC-QUEUED-YTD
           MOVE TLXM-SETTLE-AMT
             TO TXPRC-SETTLE-AMT
           MOVE TLXM-OPEN-DATE
             TO TXPRC-OPEN-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXPRC-NEXT-DUE-DATE
           MOVE TLXM-NOTICE-DATE
             TO TXPRC-NOTICE-DATE
           MOVE TLXM-EXPIRY-DATE
             TO TXPRC-EXPIRY-DATE
           MOVE TLXM-TEST-KEY-CD
             TO TXPRC-TEST-KEY-CD
           MOVE TLXM-REGION-CD
             TO TXPRC-REGION-CD
           MOVE TLXM-BRANCH-CD
             TO TXPRC-BRANCH-CD
           MOVE TLXM-REASON-CD
             TO TXPRC-REASON-CD
           MOVE TLXM-NOTICE-SENT-FLAG
             TO TXPRC-NOTICE-SENT-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXPRC-STOP-FLAG
           MOVE TLXM-REVIEW-FLAG
             TO TXPRC-REVIEW-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXPRC-PAYMENT-CNT
           MOVE TLXM-EXCEPT-CNT
             TO TXPRC-EXCEPT-CNT
           MOVE TLXM-RATE
             TO TXPRC-RATE
           MOVE TLXM-DISCOUNT
             TO TXPRC-DISCOUNT
           MOVE TLXM-ADDR
             TO TXPRC-ADDR
           MOVE TLXM-NAME
             TO TXPRC-NAME
           CALL 'TXPRC' USING TXPRC-PARMS
           EVALUATE TXPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXPRC-COVER-YTD
                     TO TLXM-COVER-YTD
                   MOVE TXPRC-QUEUED-AMT
                     TO TLXM-QUEUED-AMT
                   MOVE TXPRC-SETTLE-YTD
                     TO TLXM-SETTLE-YTD
                   MOVE TXPRC-QUEUED-YTD
                     TO TLXM-QUEUED-YTD
                   MOVE TXPRC-NOTICE-SENT-FLAG
                     TO TLXM-NOTICE-SENT-FLAG
                   MOVE TXPRC-STOP-FLAG
                     TO TLXM-STOP-FLAG
                   MOVE TXPRC-REVIEW-FLAG
                     TO TLXM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TXPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TXELG.
           INITIALIZE TXELG-PARMS
           MOVE TLXM-WIRE-YTD
             TO TXELG-WIRE-YTD
           MOVE TLXM-CHARGE-AMT
             TO TXELG-CHARGE-AMT
           MOVE TLXM-QUEUED-AMT
             TO TXELG-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXELG-CORRESP-YTD
           MOVE TLXM-CORRESP-AMT
             TO TXELG-CORRESP-AMT
           MOVE TLXM-WIRE-AMT
             TO TXELG-WIRE-AMT
           MOVE TLXM-LAST-PAY-DATE
             TO TXELG-LAST-PAY-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXELG-NEXT-DUE-DATE
           MOVE TLXM-EFFECTIVE-DATE
             TO TXELG-EFFECTIVE-DATE
           MOVE TLXM-CLOSE-DATE
             TO TXELG-CLOSE-DATE
           MOVE TLXM-MSG-TYPE-CD
             TO TXELG-MSG-TYPE-CD
           MOVE TLXM-REGION-CD
             TO TXELG-REGION-CD
           MOVE TLXM-TYPE-CD
             TO TXELG-TYPE-CD
           MOVE TLXM-REASON-CD
             TO TXELG-REASON-CD
           MOVE TLXM-SUSPEND-FLAG
             TO TXELG-SUSPEND-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXELG-STOP-FLAG
           MOVE TLXM-OVERRIDE-FLAG
             TO TXELG-OVERRIDE-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXELG-PAYMENT-CNT
           MOVE TLXM-NOTICE-CNT
             TO TXELG-NOTICE-CNT
           MOVE TLXM-FEE-PCT
             TO TXELG-FEE-PCT
           MOVE TLXM-RATE
             TO TXELG-RATE
           MOVE TLXM-ADDR
             TO TXELG-ADDR
           MOVE TLXM-DESC
             TO TXELG-DESC
           CALL 'TXELG' USING TXELG-PARMS
           EVALUATE TXELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXELG-CHARGE-AMT
                     TO TLXM-CHARGE-AMT
                   MOVE TXELG-WIRE-AMT
                     TO TLXM-WIRE-AMT
                   MOVE TXELG-SUSPEND-FLAG
                     TO TLXM-SUSPEND-FLAG
                   MOVE TXELG-STOP-FLAG
                     TO TLXM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TXELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TLXT-KEY TO RD-KEY
           MOVE TLXT-TRAN-CD TO RD-CODE
           MOVE TLXT-COVER-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TLXT-KEY TO RD-KEY
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
           DISPLAY 'TXXYREN READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TXXYREN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
