      *================================================================*
      * PROGRAM  : TCCSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TRAVELERS CHEQUES - STATEMENT PREPARATION. READS THE*
      *            TRC TRANSACTIONS, UPDATES THE TRC MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. TRAVELERS CHEQUE SALES,*
      *            CONSIGNMENT INVENTORY AND REMITTANCE                *
      * INPUT    : TRCMST (KSDS), TRCTRN, BANKCTL                      *
      * OUTPUT   : TRCOUT, TCCSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TCPRC                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-06-08 PQR  ORIGINAL                                       *
      * 1992-11-27 TWH  REMIT EXCEPTIONS TO OPS QUEUE PRB4485          *
      * 1992-08-20 LMB  SHORTAGE TABLE EXPANDED INC1616                *
      * 1996-05-27 CRP  REMIT RERUN PROTECTION INC7904                 *
      * 1996-09-14 CRP  COMMISSION EXCEPTIONS TO OPS QUEUE PRB4906     *
      * 2000-08-14 LMB  PERFORMANCE - COMMISSION READ AHEAD REQ4559    *
      * 2008-04-04 GAF  CONSIGN NOTICE TEXT PER LEGAL PRB2117          *
      * 2008-09-24 LMB  REFUND EXCEPTIONS TO OPS QUEUE CHG9748         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCCSTMT.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TRCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TRCM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TRCTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TRCOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TCCSTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TRCMST.
       FD  TRAN-FILE.
       COPY TRCTRN.
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
                                           VALUE 24.000000.
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
      * WS-REMIT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REMIT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000485000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000357000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000056000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000432000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000366000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000464000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000464000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013700000000349500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016200000000253000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000036500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017250000000035500'.
       01  WS-REMIT-BAND-TBL REDEFINES WS-REMIT-BAND-VALUES.
           05  WS-REMIT-BAND-ROW OCCURS 11 TIMES.
               10  WS-REMIT-BAND-LOW       PIC 9(09)V99.
               10  WS-REMIT-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FACE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000677500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000599000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000363500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000149000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000750000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000067000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000587000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000772500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000648500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024600000000265000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029600000000156500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039600000000571000'.
       01  WS-FACE-SCHED-TBL REDEFINES WS-FACE-SCHED-VALUES.
           05  WS-FACE-SCHED-ROW OCCURS 12 TIMES.
               10  WS-FACE-SCHED-LOW       PIC 9(09)V99.
               10  WS-FACE-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FACE-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000363500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000562500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000565000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000418000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000153500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018350000000547000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028350000000151000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028600000000655500'.
       01  WS-FACE-SCHEDX-TBL REDEFINES WS-FACE-SCHEDX-VALUES.
           05  WS-FACE-SCHEDX-ROW OCCURS 9 TIMES.
               10  WS-FACE-SCHEDX-LOW      PIC 9(09)V99.
               10  WS-FACE-SCHEDX-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHORTAGE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHORTAGE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000864500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000067500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000138000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000878500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000255500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000891500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000678500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000441000'.
       01  WS-SHORTAGE-GRID-TBL REDEFINES WS-SHORTAGE-GRID-VALUES.
           05  WS-SHORTAGE-GRID-ROW OCCURS 8 TIMES.
               10  WS-SHORTAGE-GRID-LOW    PIC 9(09)V99.
               10  WS-SHORTAGE-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TCCSTMT  TRAVELERS CHEQUES'.
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
       COPY TCPRCP.

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
               MOVE 'TRCMST' TO ABND-FILE-NAME
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
           MOVE TRCT-KEY TO TRCM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCRUE-MARGIN
               PERFORM 3010-ACCRUE-REFUND
               PERFORM 3020-DESCRIBE-ISSUER
               PERFORM 3030-SELECT-FEE-PCT
               PERFORM 3040-CHECK-EFFECTIVE
               PERFORM 3050-LIMIT-REFUND
               PERFORM 3060-COUNT-RETRY
               PERFORM 3070-AGE-LAST-PAY
               PERFORM 3080-ACCRUE-FEE-PCT
               PERFORM 3090-EDIT-DESC
               PERFORM 3100-ACCRUE-CONSIGN
               PERFORM 3110-SET-CHANNEL
               PERFORM 3120-SET-CYCLE
               PERFORM 3130-COUNT-SEQ
               PERFORM 3140-SPLIT-REFUND
               PERFORM 3150-ACCRUE-REFUND
               PERFORM 3160-PRORATE-REFUND
               PERFORM 3170-SPLIT-REMIT
               PERFORM 3180-AGE-CLOSE
               PERFORM 3190-SELECT-RATE
               PERFORM 3200-ASSESS-NOTICE-SENT
               PERFORM 3210-SET-DENOM
               PERFORM 3220-ACCUM-CHANNEL
               PERFORM 3230-ACCRUE-CONSIGN
               PERFORM 3240-DESCRIBE-STATUS
               PERFORM 3250-SELECT-MARGIN
               PERFORM 3260-LIMIT-REFUND
               PERFORM 3270-SELECT-MARGIN
               PERFORM 3280-DESCRIBE-TRAN
               PERFORM 3290-EDIT-DESC
               PERFORM 3300-ACCUM-SOURCE
               PERFORM 3310-FLAG-ITEM
               PERFORM 3320-LIMIT-FACE
               PERFORM 3330-FLAG-COMMISSION
               PERFORM 3340-SELECT-MARGIN
               PERFORM 3350-CHECK-CLOSE
               PERFORM 3360-PRORATE-CONSIGN
               PERFORM 3370-ACCUM-CHANNEL
               PERFORM 3380-FLAG-CONSIGN
               PERFORM 3390-COUNT-EXCEPT
               PERFORM 3400-SET-CYCLE
               PERFORM 3410-DESCRIBE-SOURCE
               PERFORM 3420-AGE-LAST-STMT
               PERFORM 3430-COUNT-SEQ
               PERFORM 3440-ACCRUE-COMMISSION
               PERFORM 3450-SET-TRAN
               PERFORM 3460-FLAG-REMIT
               PERFORM 3470-FLAG-REFUND
               PERFORM 3480-ACCRUE-COMMISSION
               PERFORM 3490-PRORATE-CONSIGN
               PERFORM 3500-AGE-EFF
               PERFORM 3510-EDIT-ADDR
               PERFORM 3520-ASSESS-REVERSAL
               PERFORM 3530-FLAG-CONSIGN
               PERFORM 3540-SELECT-RATE
               PERFORM 3550-COUNT-RETRY
               PERFORM 3560-ASSESS-STOP
               PERFORM 3570-ACCRUE-FEE-PCT
               PERFORM 3580-ACCUM-BRANCH
               PERFORM 3590-SET-BRANCH
               PERFORM 3600-CHECK-EFFECTIVE
               PERFORM 3610-SPLIT-REMIT
               PERFORM 3620-SET-CHANNEL
               PERFORM 3630-SET-ISSUER
               PERFORM 3640-SPLIT-REMIT
               PERFORM 3650-COUNT-CYCLE
               PERFORM 3660-COUNT-CYCLE
               PERFORM 3670-EDIT-CUST-ID
               PERFORM 3680-COUNT-RETRY
               PERFORM 3690-DESCRIBE-ISSUER
               PERFORM 3700-SPLIT-COMMISSION
               PERFORM 3710-LIMIT-FACE
               PERFORM 3720-CHECK-LAST-STMT
               PERFORM 3730-LIMIT-FACE
               PERFORM 3740-ASSESS-REVERSAL
               PERFORM 3750-SPLIT-FACE
               PERFORM 3760-LIMIT-COMMISSION
               PERFORM 3770-ACCRUE-CONSIGN
               PERFORM 3780-ACCUM-SOURCE
               PERFORM 3790-SPLIT-REMIT
               PERFORM 3800-DESCRIBE-CYCLE
               PERFORM 3810-PRORATE-REFUND
               PERFORM 3820-ASSESS-EXEMPT
               PERFORM 3830-PRORATE-FACE
               PERFORM 3840-FLAG-ITEM
               PERFORM 3850-CHECK-EFF
               PERFORM 3860-PRORATE-COMMISSION
               PERFORM 3870-ACCRUE-CONSIGN
               PERFORM 3880-ACCUM-BRANCH
               PERFORM 3890-SET-SOURCE
               PERFORM 3900-LIMIT-COMMISSION
               PERFORM 3910-LIMIT-CONSIGN
               PERFORM 3920-AGE-LAST-PAY
               PERFORM 3930-LIMIT-SHORTAGE
               PERFORM 3940-LIMIT-REFUND
               PERFORM 3950-ACCRUE-REMIT
               PERFORM 3960-LIMIT-REFUND
               PERFORM 3970-ACCRUE-FACE
               PERFORM 3980-COUNT-SEQ
               PERFORM 3990-FLAG-ITEM
               PERFORM 4000-LIMIT-COMMISSION
               PERFORM 4010-SELECT-RATE
               PERFORM 4020-FLAG-REFUND
               PERFORM 4030-CHECK-EFFECTIVE
               PERFORM 4040-AGE-LAST-ACT
               PERFORM 4050-EDIT-REF-NO
               PERFORM 4060-ACCRUE-COMMISSION
               PERFORM 4070-PRORATE-REMIT
               PERFORM 4080-SPLIT-REMIT
               PERFORM 4090-ACCUM-CYCLE
               PERFORM 4100-AGE-LAST-STMT
               PERFORM 4110-EDIT-CUST-ID
               PERFORM 4120-ACCRUE-RATE
               PERFORM 4130-ACCUM-BRANCH
               PERFORM 4140-FLAG-REFUND
               PERFORM 4150-FLAG-REFUND
               PERFORM 4160-CHECK-EFF
               PERFORM 4170-SELECT-FEE-PCT
               PERFORM 4180-EDIT-OPERATOR
               PERFORM 4190-ACCUM-BRANCH
               PERFORM 4200-SPLIT-COMMISSION
               PERFORM 4210-SELECT-FEE-PCT
               PERFORM 4220-DESCRIBE-ISSUER
               PERFORM 4230-SELECT-MARGIN
               PERFORM 4240-ASSESS-NOTICE-SENT
               PERFORM 4250-DESCRIBE-STATUS
               PERFORM 4260-FLAG-COMMISSION
               PERFORM 4270-SELECT-FEE-PCT
               PERFORM 4280-CHECK-NEXT-DUE
               PERFORM 4290-SPLIT-COMMISSION
               PERFORM 4300-DESCRIBE-SOURCE
               PERFORM 4310-EDIT-CONTACT
               PERFORM 4320-LIMIT-COMMISSION
               PERFORM 4330-SPLIT-COMMISSION
               PERFORM 4340-COUNT-SEQ
               PERFORM 4350-SET-BRANCH
               PERFORM 4360-PRORATE-CONSIGN
               PERFORM 4370-CHECK-LAST-STMT
               PERFORM 4380-COUNT-CYCLE
               PERFORM 4390-AGE-LAST-STMT
               PERFORM 4400-ACCRUE-FEE-PCT
               PERFORM 4410-ACCUM-ISSUER
               PERFORM 4420-ACCUM-SOURCE
               PERFORM 4430-ASSESS-SUSPEND
               PERFORM 4440-ACCUM-CHANNEL
               PERFORM 4450-SPLIT-REFUND
               PERFORM 4460-ACCRUE-REMIT
               PERFORM 4470-SPLIT-COMMISSION
               PERFORM 4480-ASSESS-SUSPEND
               PERFORM 4490-AGE-NEXT-DUE
               PERFORM 4500-DESCRIBE-BRANCH
               PERFORM 4510-EDIT-NAME
               PERFORM 4520-FLAG-FACE
               PERFORM 4530-AGE-NEXT-DUE
               PERFORM 4540-PRORATE-CONSIGN
               PERFORM 4550-FLAG-FACE
               PERFORM 4560-ASSESS-SUSPEND
               PERFORM 4570-EDIT-REF-NO
               PERFORM 4580-SELECT-MARGIN
               PERFORM 4590-ACCRUE-COMMISSION
               PERFORM 4600-EDIT-DESC
               PERFORM 4610-LIMIT-CONSIGN
               PERFORM 4620-SPLIT-REFUND
               PERFORM 4630-PRORATE-COMMISSION
               PERFORM 4640-LIMIT-SHORTAGE
               PERFORM 4650-SET-CHANNEL
               PERFORM 4660-SET-CYCLE
               PERFORM 4670-COUNT-CYCLE
               PERFORM 4680-LIMIT-CONSIGN
               PERFORM 4690-PRORATE-REFUND
               PERFORM 4700-PRORATE-REMIT
               PERFORM 4710-AGE-LAST-PAY
               PERFORM 4720-DESCRIBE-ISSUER
               PERFORM 4730-FLAG-CONSIGN
               PERFORM 4740-ACCRUE-CONSIGN
               PERFORM 4750-SPLIT-COMMISSION
               PERFORM 7020-CALL-TCPRC
               REWRITE TRC-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TRCMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TRC-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-REFUND.
           IF TRCT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-REFUND-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE GRID TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-GRID-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRCM-SHORTAGE-YTD NOT < WS-SHORTAGE-GRID-LOW(WS-TX)
                   MOVE WS-SHORTAGE-GRID-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-REFUND.
           IF TRCM-REFUND-YTD < 100.00
               MOVE 100.00 TO TRCM-REFUND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REFUND-YTD > 1000.00
               MOVE 1000.00 TO TRCM-REFUND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TRCM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E435 DESC REQUIRED
      * E044 FACE YTD NEGATIVE
      * E381 FACE YTD OVER 99999
      * E229 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-DESC = SPACES
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN TRCM-FACE-YTD < 0
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN TRCM-FACE-YTD > 99999.00
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN TRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3100-ACCRUE-CONSIGN.
           IF TRCT-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-CONSIGN-AMT * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-CHANNEL.
           EVALUATE TRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EFFECTIVE DATE IS PAST
      * HHK -> JCB WHEN EFFECTIVE DATE IS PAST
      * JCB -> UCB WHEN EFFECTIVE DATE IS PAST
      * UCB -> XTY WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-SEQ.
           IF TRCT-SEQ < 9999999
               ADD 1 TO TRCT-SEQ
           END-IF
           IF TRCT-SEQ NOT < 10
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCT-REFUND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRCT-REFUND-AMT - (WS-WORK-AMT * 3)
           COMPUTE TRCM-FACE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-REFUND.
           IF TRCT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-REFUND-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-COMMISSION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONSIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REFUND-YTD ROUNDED =
                   TRCM-CONSIGN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-REFUND-YTD
           END-IF
           ADD TRCM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCM-REMIT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TRCM-REMIT-YTD - (WS-WORK-AMT * 3)
           COMPUTE TRCT-REMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE COMMISSION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-COMMISSION-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE CONSIGN AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3200-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRCT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCT-REFUND-AMT ROUNDED = TRCT-CONSIGN-AMT *
                       TRCM-RATE / 200
               WHEN TRCM-DENOM-CD = 'G'
                   MOVE 7.00 TO TRCT-REFUND-AMT
               WHEN TRCM-DENOM-CD = 'M'
                   MOVE 7.00 TO TRCT-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCT-REFUND-AMT ROUNDED = TRCT-CONSIGN-AMT *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCT-REFUND-AMT > 0 AND TRCT-REFUND-AMT < 5.00
               MOVE 5.00 TO TRCT-REFUND-AMT
           END-IF
           IF TRCT-REFUND-AMT > 75.00
               MOVE 75.00 TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * G -> M WHEN POST DATE IS PAST
      * M -> N WHEN POST DATE IS PAST
      * N -> P WHEN POST DATE IS PAST
      * P -> V WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-DENOM.
           EVALUATE TRCM-DENOM-CD
               WHEN 'G'
                   IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-CHANNEL-CD
                   ADD TRCM-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-CONSIGN.
           IF TRCM-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-CONSIGN-AMT * TRCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRCT-CONSIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE FACE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-FACE-YTD NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-REFUND.
           IF TRCM-REFUND-AMT < 100.00
               MOVE 100.00 TO TRCM-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REFUND-AMT > 50000.00
               MOVE 50000.00 TO TRCM-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE GRID TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-GRID-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRCT-REMIT-AMT NOT < WS-SHORTAGE-GRID-LOW(WS-TX)
                   MOVE WS-SHORTAGE-GRID-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E956 DESC REQUIRED
      * E955 REFUND YTD NEGATIVE
      * E752 REFUND YTD OVER 5000
      * E777 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3290-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-DESC = SPACES
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-YTD < 0
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-YTD > 5000.00
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-SOURCE-CD
                   ADD TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3310-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-FACE.
           IF TRCM-FACE-AMT < 100.00
               MOVE 100.00 TO TRCM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-FACE-AMT > 1000.00
               MOVE 1000.00 TO TRCM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3330-FLAG-COMMISSION.
           IF TRCM-COMMISSION-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-COMMISSION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE GRID TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-GRID-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRCM-REMIT-AMT NOT < WS-SHORTAGE-GRID-LOW(WS-TX)
                   MOVE WS-SHORTAGE-GRID-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-YTD ROUNDED =
                   TRCM-REMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-CHANNEL-CD
                   ADD TRCM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3380-FLAG-CONSIGN.
           IF TRCM-CONSIGN-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN NEXT REVIEW DATE IS PAST
      * HHK -> JCB WHEN NEXT REVIEW DATE IS PAST
      * JCB -> UCB WHEN NEXT REVIEW DATE IS PAST
      * UCB -> XTY WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   IF TRCM-NEXT-REVIEW-DATE > 0 AND
                       TRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TRCM-NEXT-REVIEW-DATE > 0 AND
                       TRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TRCM-NEXT-REVIEW-DATE > 0 AND
                       TRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TRCM-NEXT-REVIEW-DATE > 0 AND
                       TRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-SEQ.
           IF TRCT-SEQ < 9999999
               ADD 1 TO TRCT-SEQ
           END-IF
           IF TRCT-SEQ NOT < 12
               MOVE 'Y' TO TRCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-COMMISSION.
           IF TRCT-COMMISSION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-COMMISSION-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCM-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SL -> RF WHEN EXPIRY DATE IS PAST
      * RF -> RM WHEN EXPIRY DATE IS PAST
      * RM -> CN WHEN EXPIRY DATE IS PAST
      * CN -> VD WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RM'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REMIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3460-FLAG-REMIT.
           IF TRCM-REMIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-REMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3470-FLAG-REFUND.
           IF TRCM-REFUND-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-COMMISSION.
           IF TRCM-COMMISSION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-COMMISSION-YTD * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TRCM-CONSIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-AMT ROUNDED =
                   TRCM-REMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-AMT
           END-IF
           ADD TRCM-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCM-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E909 ADDR REQUIRED
      * E646 REMIT AMT NEGATIVE
      * E096 REMIT AMT OVER 99999
      * E572 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-ADDR = SPACES
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN TRCM-REMIT-AMT < 0
                   MOVE 'E646' TO WS-REJECT-CODE
               WHEN TRCM-REMIT-AMT > 99999.00
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN TRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E572' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONSIGN FEE ON THE REFUND YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3520-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCT-CONSIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCT-CONSIGN-AMT ROUNDED = TRCM-REFUND-YTD *
                       TRCM-RATE / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 20.00 TO TRCT-CONSIGN-AMT
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 7.00 TO TRCT-CONSIGN-AMT
               WHEN OTHER
                   COMPUTE TRCT-CONSIGN-AMT ROUNDED = TRCM-REFUND-YTD *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCT-CONSIGN-AMT > 0 AND TRCT-CONSIGN-AMT < 10.00
               MOVE 10.00 TO TRCT-CONSIGN-AMT
           END-IF
           IF TRCT-CONSIGN-AMT > 25.00
               MOVE 25.00 TO TRCT-CONSIGN-AMT
           END-IF
           ADD TRCT-CONSIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3530-FLAG-CONSIGN.
           IF TRCM-CONSIGN-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHEDX TIER FOR THE FACE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHEDX-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCM-FACE-YTD NOT < WS-FACE-SCHEDX-LOW(WS-TX)
                   MOVE WS-FACE-SCHEDX-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3560-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRCM-REFUND-AMT ROUNDED = TRCM-SHORTAGE-YTD *
                       TRCM-RATE / 200
               WHEN TRCM-DENOM-CD = 'G'
                   MOVE 7.00 TO TRCM-REFUND-AMT
               WHEN TRCM-DENOM-CD = 'M'
                   MOVE 12.00 TO TRCM-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCM-REFUND-AMT ROUNDED = TRCM-SHORTAGE-YTD *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-REFUND-AMT > 0 AND TRCM-REFUND-AMT < 2.00
               MOVE 2.00 TO TRCM-REFUND-AMT
           END-IF
           IF TRCM-REFUND-AMT > 25.00
               MOVE 25.00 TO TRCM-REFUND-AMT
           END-IF
           ADD TRCM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TRCM-REMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-BRANCH-CD
                   ADD TRCM-COMMISSION-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-COMMISSION-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EG -> HZ WHEN EXPIRY DATE IS PAST
      * HZ -> JF WHEN EXPIRY DATE IS PAST
      * JF -> NZ WHEN EXPIRY DATE IS PAST
      * NZ -> EG WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HZ' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HZ'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EG' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCT-REMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRCT-REMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-CHANNEL.
           EVALUATE TRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN EFFECTIVE DATE IS PAST
      * CJ -> DU WHEN EFFECTIVE DATE IS PAST
      * DU -> JY WHEN EFFECTIVE DATE IS PAST
      * JY -> RR WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RR' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCM-REMIT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-REMIT-YTD - (WS-WORK-AMT * 4)
           COMPUTE TRCM-CONSIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO TRCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E839 CUST ID REQUIRED
      * E946 SHORTAGE YTD NEGATIVE
      * E940 SHORTAGE YTD OVER 50000
      * E488 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-CUST-ID = SPACES
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-YTD < 0
                   MOVE 'E946' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-YTD > 50000.00
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3710-LIMIT-FACE.
           IF TRCM-FACE-AMT < 25.00
               MOVE 25.00 TO TRCM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-FACE-AMT > 2500.00
               MOVE 2500.00 TO TRCM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-FACE.
           IF TRCM-FACE-YTD < 0.00
               MOVE 0.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-FACE-YTD > 5000.00
               MOVE 5000.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3740-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRCM-REFUND-AMT ROUNDED = TRCM-SHORTAGE-YTD *
                       TRCM-RATE / 200
               WHEN TRCM-STATUS-CD = 'HK'
                   MOVE 7.00 TO TRCM-REFUND-AMT
               WHEN TRCM-STATUS-CD = 'RU'
                   MOVE 20.00 TO TRCM-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCM-REFUND-AMT ROUNDED = TRCM-SHORTAGE-YTD *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-REFUND-AMT > 0 AND TRCM-REFUND-AMT < 5.00
               MOVE 5.00 TO TRCM-REFUND-AMT
           END-IF
           IF TRCM-REFUND-AMT > 75.00
               MOVE 75.00 TO TRCM-REFUND-AMT
           END-IF
           ADD TRCM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3750-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = TRCM-FACE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-FACE-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-COMMISSION.
           IF TRCT-COMMISSION-AMT < 50.00
               MOVE 50.00 TO TRCT-COMMISSION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-COMMISSION-AMT > 1000.00
               MOVE 1000.00 TO TRCT-COMMISSION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-CONSIGN.
           IF TRCM-CONSIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-CONSIGN-YTD * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCM-REMIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRCM-REMIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3810-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRCM-REFUND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE REFUND YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3820-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCM-FACE-YTD ROUNDED = TRCM-REFUND-YTD *
                       TRCM-RATE / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 3.00 TO TRCM-FACE-YTD
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 12.00 TO TRCM-FACE-YTD
               WHEN OTHER
                   COMPUTE TRCM-FACE-YTD ROUNDED = TRCM-REFUND-YTD *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-FACE-YTD > 0 AND TRCM-FACE-YTD < 5.00
               MOVE 5.00 TO TRCM-FACE-YTD
           END-IF
           IF TRCM-FACE-YTD > 100.00
               MOVE 100.00 TO TRCM-FACE-YTD
           END-IF
           ADD TRCM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       3830-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-FACE-YTD ROUNDED =
                   TRCM-SHORTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-FACE-YTD
           END-IF
           ADD TRCM-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3840-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRCT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COMMISSION YTD.
      *----------------------------------------------------------------*
       3860-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-COMMISSION-YTD ROUNDED =
                   TRCM-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-COMMISSION-YTD
           END-IF
           ADD TRCM-COMMISSION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-CONSIGN.
           IF TRCM-CONSIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-CONSIGN-YTD * TRCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TRCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * EG -> GH WHEN EFFECTIVE DATE IS PAST
      * GH -> LC WHEN EFFECTIVE DATE IS PAST
      * LC -> LU WHEN EFFECTIVE DATE IS PAST
      * LU -> YS WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GH' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GH'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LU' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LU'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COMMISSION YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-COMMISSION.
           IF TRCM-COMMISSION-YTD < 50.00
               MOVE 50.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-COMMISSION-YTD > 2500.00
               MOVE 2500.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3910-LIMIT-CONSIGN.
           IF TRCM-CONSIGN-YTD < 0.00
               MOVE 0.00 TO TRCM-CONSIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-CONSIGN-YTD > 50000.00
               MOVE 50000.00 TO TRCM-CONSIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-YTD < 0.00
               MOVE 0.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-YTD > 5000.00
               MOVE 5000.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3940-LIMIT-REFUND.
           IF TRCM-REFUND-AMT < 100.00
               MOVE 100.00 TO TRCM-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REFUND-AMT > 5000.00
               MOVE 5000.00 TO TRCM-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-REMIT.
           IF TRCM-REMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-YTD * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-COMMISSION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-REFUND.
           IF TRCM-REFUND-AMT < 25.00
               MOVE 25.00 TO TRCM-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REFUND-AMT > 50000.00
               MOVE 50000.00 TO TRCM-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-FACE.
           IF TRCM-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-FACE-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCM-COMMISSION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-SEQ.
           IF TRCT-SEQ < 9999999
               ADD 1 TO TRCT-SEQ
           END-IF
           IF TRCT-SEQ NOT < 10
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3990-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4000-LIMIT-COMMISSION.
           IF TRCT-COMMISSION-AMT < 100.00
               MOVE 100.00 TO TRCT-COMMISSION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-COMMISSION-AMT > 1000.00
               MOVE 1000.00 TO TRCT-COMMISSION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE GRID TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-GRID-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRCM-REFUND-AMT NOT < WS-SHORTAGE-GRID-LOW(WS-TX)
                   MOVE WS-SHORTAGE-GRID-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4020-FLAG-REFUND.
           IF TRCM-REFUND-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-COMMISSION-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-COMMISSION-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-COMMISSION-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-COMMISSION-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-COMMISSION-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E029 REF NO REQUIRED
      * E743 SHORTAGE YTD NEGATIVE
      * E070 SHORTAGE YTD OVER 1000000
      * E649 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-REF-NO = SPACES
                   MOVE 'E029' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-YTD < 0
                   MOVE 'E743' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-YTD > 1000000.00
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-COMMISSION.
           IF TRCT-COMMISSION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-COMMISSION-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       4070-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-REMIT-AMT ROUNDED =
                   TRCM-COMMISSION-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCT-REMIT-AMT
           END-IF
           ADD TRCT-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCT-REMIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRCT-REMIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE TRCM-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-CYCLE-CD
                   ADD TRCM-CONSIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-CONSIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4100-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E210 CUST ID REQUIRED
      * E583 SHORTAGE AMT NEGATIVE
      * E033 SHORTAGE AMT OVER 250000
      * E936 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-CUST-ID = SPACES
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-AMT < 0
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-AMT > 250000.00
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN TRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-BRANCH-CD
                   ADD TRCM-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4140-FLAG-REFUND.
           IF TRCM-REFUND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4150-FLAG-REFUND.
           IF TRCM-REFUND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRCT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHEDX TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHEDX-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCM-REMIT-AMT NOT < WS-FACE-SCHEDX-LOW(WS-TX)
                   MOVE WS-FACE-SCHEDX-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E794 OPERATOR REQUIRED
      * E800 REMIT YTD NEGATIVE
      * E439 REMIT YTD OVER 250000
      * E482 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-OPERATOR = SPACES
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN TRCM-REMIT-YTD < 0
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN TRCM-REMIT-YTD > 250000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN TRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-BRANCH-CD
                   ADD TRCT-CONSIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-CONSIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TRCM-CONSIGN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT BAND TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-BAND-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRCM-FACE-AMT NOT < WS-REMIT-BAND-LOW(WS-TX)
                   MOVE WS-REMIT-BAND-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE GRID TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-GRID-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TRCM-SHORTAGE-YTD NOT < WS-SHORTAGE-GRID-LOW(WS-TX)
                   MOVE WS-SHORTAGE-GRID-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4240-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TRCM-REFUND-YTD ROUNDED = WS-WORK-AMT *
                       TRCM-MARGIN / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 3.00 TO TRCM-REFUND-YTD
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 7.00 TO TRCM-REFUND-YTD
               WHEN OTHER
                   COMPUTE TRCM-REFUND-YTD ROUNDED = WS-WORK-AMT *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-REFUND-YTD > 0 AND TRCM-REFUND-YTD < 5.00
               MOVE 5.00 TO TRCM-REFUND-YTD
           END-IF
           IF TRCM-REFUND-YTD > 150.00
               MOVE 150.00 TO TRCM-REFUND-YTD
           END-IF
           ADD TRCM-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COMMISSION AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4260-FLAG-COMMISSION.
           IF TRCM-COMMISSION-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-COMMISSION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE SCHED TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-SCHED-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-SHORTAGE-AMT NOT < WS-FACE-SCHED-LOW(WS-TX)
                   MOVE WS-FACE-SCHED-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4280-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCT-COMMISSION-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRCT-COMMISSION-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TRCM-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4300-DESCRIBE-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E253 CONTACT REQUIRED
      * E788 COMMISSION YTD NEGATIVE
      * E546 COMMISSION YTD OVER 10000
      * E254 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-CONTACT = SPACES
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD < 0
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD > 10000.00
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E254' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4320-LIMIT-COMMISSION.
           IF TRCM-COMMISSION-YTD < 0.00
               MOVE 0.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-COMMISSION-YTD > 10000.00
               MOVE 10000.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4330-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRCT-COMMISSION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4340-COUNT-SEQ.
           IF TRCT-SEQ < 9999999
               ADD 1 TO TRCT-SEQ
           END-IF
           IF TRCT-SEQ NOT < 6
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EG -> HZ WHEN NEXT DUE DATE IS PAST
      * HZ -> JF WHEN NEXT DUE DATE IS PAST
      * JF -> NZ WHEN NEXT DUE DATE IS PAST
      * NZ -> EG WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4350-SET-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HZ' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HZ'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EG' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONSIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN YTD.
      *----------------------------------------------------------------*
       4360-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-YTD ROUNDED =
                   TRCT-CONSIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4370-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4380-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO TRCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4390-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRCM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TRCM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4410-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-ISSUER-CD
                   ADD TRCT-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4420-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-SOURCE-CD
                   ADD TRCM-COMMISSION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-COMMISSION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT YTD FEE ON THE COMMISSION AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4430-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TRCM-REMIT-YTD ROUNDED = TRCM-COMMISSION-AMT
                       * TRCM-FEE-PCT / 200
               WHEN TRCM-DENOM-CD = 'G'
                   MOVE 12.00 TO TRCM-REMIT-YTD
               WHEN TRCM-DENOM-CD = 'M'
                   MOVE 15.00 TO TRCM-REMIT-YTD
               WHEN OTHER
                   COMPUTE TRCM-REMIT-YTD ROUNDED = TRCM-COMMISSION-AMT
                       * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-REMIT-YTD > 0 AND TRCM-REMIT-YTD < 5.00
               MOVE 5.00 TO TRCM-REMIT-YTD
           END-IF
           IF TRCM-REMIT-YTD > 100.00
               MOVE 100.00 TO TRCM-REMIT-YTD
           END-IF
           ADD TRCM-REMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-CHANNEL-CD
                   ADD TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCT-REFUND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRCT-REFUND-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRCT-REMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4460-ACCRUE-REMIT.
           IF TRCM-REMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-YTD * TRCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRCT-COMMISSION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TRCT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE COMMISSION AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4480-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCM-SHORTAGE-YTD ROUNDED =
                       TRCT-COMMISSION-AMT * TRCM-FEE-PCT / 200
               WHEN TRCM-ISSUER-CD = 'AF'
                   MOVE 3.00 TO TRCM-SHORTAGE-YTD
               WHEN TRCM-ISSUER-CD = 'CJ'
                   MOVE 10.00 TO TRCM-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE TRCM-SHORTAGE-YTD ROUNDED =
                       TRCT-COMMISSION-AMT * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-SHORTAGE-YTD > 0 AND TRCM-SHORTAGE-YTD < 5.00
               MOVE 5.00 TO TRCM-SHORTAGE-YTD
           END-IF
           IF TRCM-SHORTAGE-YTD > 250.00
               MOVE 250.00 TO TRCM-SHORTAGE-YTD
           END-IF
           ADD TRCM-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4490-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4500-DESCRIBE-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E229 NAME REQUIRED
      * E140 WORK AMT 2 NEGATIVE
      * E679 WORK AMT 2 OVER 99999
      * E027 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4510-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-NAME = SPACES
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E679' TO WS-REJECT-CODE
               WHEN TRCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4520-FLAG-FACE.
           IF TRCM-FACE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4530-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN YTD.
      *----------------------------------------------------------------*
       4540-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FACE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4550-FLAG-FACE.
           IF TRCM-FACE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE CONSIGN AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4560-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-STOP-FLAG = 'Y'
                   COMPUTE TRCM-REFUND-AMT ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-RATE / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 3.00 TO TRCM-REFUND-AMT
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 3.00 TO TRCM-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCM-REFUND-AMT ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-REFUND-AMT > 0 AND TRCM-REFUND-AMT < 1.00
               MOVE 1.00 TO TRCM-REFUND-AMT
           END-IF
           IF TRCM-REFUND-AMT > 150.00
               MOVE 150.00 TO TRCM-REFUND-AMT
           END-IF
           ADD TRCM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E764 REF NO REQUIRED
      * E377 SHORTAGE AMT NEGATIVE
      * E981 SHORTAGE AMT OVER 50000
      * E307 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4570-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-REF-NO = SPACES
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-AMT < 0
                   MOVE 'E377' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-AMT > 50000.00
                   MOVE 'E981' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E307' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT BAND TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4580-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-BAND-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TRCM-FACE-AMT NOT < WS-REMIT-BAND-LOW(WS-TX)
                   MOVE WS-REMIT-BAND-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4590-ACCRUE-COMMISSION.
           IF TRCM-COMMISSION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-COMMISSION-AMT * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TRCT-COMMISSION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E931 DESC REQUIRED
      * E409 WORK AMT 2 NEGATIVE
      * E760 WORK AMT 2 OVER 250000
      * E528 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4600-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-DESC = SPACES
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN TRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4610-LIMIT-CONSIGN.
           IF TRCM-CONSIGN-AMT < 100.00
               MOVE 100.00 TO TRCM-CONSIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-CONSIGN-AMT > 1000.00
               MOVE 1000.00 TO TRCM-CONSIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4620-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCM-REFUND-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRCM-REFUND-YTD - (WS-WORK-AMT * 2)
           COMPUTE TRCT-REMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COMMISSION YTD.
      *----------------------------------------------------------------*
       4630-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-COMMISSION-YTD ROUNDED =
                   TRCM-REMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-COMMISSION-YTD
           END-IF
           ADD TRCM-COMMISSION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4640-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-YTD < 25.00
               MOVE 25.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-YTD > 500.00
               MOVE 500.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4650-SET-CHANNEL.
           EVALUATE TRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EXPIRY DATE IS PAST
      * HHK -> JCB WHEN EXPIRY DATE IS PAST
      * JCB -> UCB WHEN EXPIRY DATE IS PAST
      * UCB -> XTY WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4660-SET-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4670-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4680-LIMIT-CONSIGN.
           IF TRCM-CONSIGN-YTD < 0.00
               MOVE 0.00 TO TRCM-CONSIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-CONSIGN-YTD > 2500.00
               MOVE 2500.00 TO TRCM-CONSIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONSIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       4690-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-REFUND-AMT ROUNDED =
                   TRCM-CONSIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       4700-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REMIT-AMT ROUNDED =
                   TRCM-COMMISSION-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4710-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4720-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4730-FLAG-CONSIGN.
           IF TRCM-CONSIGN-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4740-ACCRUE-CONSIGN.
           IF TRCT-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-CONSIGN-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4750-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCT-COMMISSION-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCT-COMMISSION-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TRC FIELDS TO TCPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TCPRC.
           INITIALIZE TCPRC-PARMS
           MOVE TRCM-REMIT-YTD
             TO TCPRC-REMIT-YTD
           MOVE TRCM-REFUND-YTD
             TO TCPRC-REFUND-YTD
           MOVE TRCM-CONSIGN-YTD
             TO TCPRC-CONSIGN-YTD
           MOVE TRCM-SHORTAGE-YTD
             TO TCPRC-SHORTAGE-YTD
           MOVE TRCM-FACE-YTD
             TO TCPRC-FACE-YTD
           MOVE TRCM-CONSIGN-AMT
             TO TCPRC-CONSIGN-AMT
           MOVE TRCM-MATURITY-DATE
             TO TCPRC-MATURITY-DATE
           MOVE TRCM-CLOSE-DATE
             TO TCPRC-CLOSE-DATE
           MOVE TRCM-NEXT-REVIEW-DATE
             TO TCPRC-NEXT-REVIEW-DATE
           MOVE TRCM-EXPIRY-DATE
             TO TCPRC-EXPIRY-DATE
           MOVE TRCM-STATUS-CD
             TO TCPRC-STATUS-CD
           MOVE TRCM-BRANCH-CD
             TO TCPRC-BRANCH-CD
           MOVE TRCM-CYCLE-CD
             TO TCPRC-CYCLE-CD
           MOVE TRCM-ISSUER-CD
             TO TCPRC-ISSUER-CD
           MOVE TRCM-REVIEW-FLAG
             TO TCPRC-REVIEW-FLAG
           MOVE TRCM-NOTICE-SENT-FLAG
             TO TCPRC-NOTICE-SENT-FLAG
           MOVE TRCM-SUSPEND-FLAG
             TO TCPRC-SUSPEND-FLAG
           MOVE TRCM-EXCEPT-CNT
             TO TCPRC-EXCEPT-CNT
           MOVE TRCM-CYCLE-CNT
             TO TCPRC-CYCLE-CNT
           MOVE TRCM-FEE-PCT
             TO TCPRC-FEE-PCT
           MOVE TRCM-MARGIN
             TO TCPRC-MARGIN
           MOVE TRCM-NAME
             TO TCPRC-NAME
           MOVE TRCM-CONTACT
             TO TCPRC-CONTACT
           CALL 'TCPRC' USING TCPRC-PARMS
           EVALUATE TCPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TCPRC-REFUND-YTD
                     TO TRCM-REFUND-YTD
                   MOVE TCPRC-CONSIGN-YTD
                     TO TRCM-CONSIGN-YTD
                   MOVE TCPRC-FACE-YTD
                     TO TRCM-FACE-YTD
                   MOVE TCPRC-NOTICE-SENT-FLAG
                     TO TRCM-NOTICE-SENT-FLAG
                   MOVE TCPRC-SUSPEND-FLAG
                     TO TRCM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TCPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TRCT-KEY TO RD-KEY
           MOVE TRCT-TRAN-CD TO RD-CODE
           MOVE TRCT-COMMISSION-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TRCT-KEY TO RD-KEY
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
           DISPLAY 'TCCSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TCCSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
