      *================================================================*
      * PROGRAM  : TCCSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TRAVELERS CHEQUES - SETTLEMENT. READS THE TRC       *
      *            TRANSACTIONS, UPDATES THE TRC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TRAVELERS CHEQUE SALES,         *
      *            CONSIGNMENT INVENTORY AND REMITTANCE                *
      * INPUT    : TRCMST (KSDS), TRCTRN, BANKCTL                      *
      * OUTPUT   : TRCOUT, TCCSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TCCAL, TCPRC, TCDTE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-07-05 TWH  ORIGINAL                                       *
      * 1987-10-13 LMB  AUDIT FINDING - FACE CONTROL TOTALS CHG7322    *
      * 1988-06-15 JLM  REFUND LIMIT REVISED REQ1802                   *
      * 1991-10-16 BHS  AUDIT FINDING - FACE CONTROL TOTALS PRB1289    *
      * 1992-04-04 TWH  REFUND CUTOFF MOVED CHG5768                    *
      * 1994-06-21 PQR  REMIT RERUN PROTECTION PRB5422                 *
      * 2006-09-16 LMB  CONSIGN FEE SCHEDULE UPDATE REQ3255            *
      * 2010-09-02 EWS  REFUND FEE SCHEDULE UPDATE INC6473             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCCSETL.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "TCCSERPT"
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
                                           VALUE 19.000000.
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
      * WS-SHORTAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHORTAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000878500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000389000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000680000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000262000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000401000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000254000'.
       01  WS-SHORTAGE-SCHED-TBL REDEFINES WS-SHORTAGE-SCHED-VALUES.
           05  WS-SHORTAGE-SCHED-ROW OCCURS 6 TIMES.
               10  WS-SHORTAGE-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHORTAGE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REMIT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REMIT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000028500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000135500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000321000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000360000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000821500'.
       01  WS-REMIT-TIER-TBL REDEFINES WS-REMIT-TIER-VALUES.
           05  WS-REMIT-TIER-ROW OCCURS 5 TIMES.
               10  WS-REMIT-TIER-LOW       PIC 9(09)V99.
               10  WS-REMIT-TIER-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TCCSETL  TRAVELERS CHEQUES'.
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
       COPY TCCALP.
       COPY TCPRCP.
       COPY TCDTEP.

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
               PERFORM 3000-CHECK-NEXT-DUE
               PERFORM 3010-COUNT-EXCEPT
               PERFORM 3020-ASSESS-NOTICE-SENT
               PERFORM 3030-SET-CHANNEL
               PERFORM 3040-SET-ISSUER
               PERFORM 3050-AGE-EXPIRY
               PERFORM 3060-LIMIT-COMMISSION
               PERFORM 3070-EDIT-REF-NO
               PERFORM 3080-ACCUM-CYCLE
               PERFORM 3090-PRORATE-FACE
               PERFORM 3100-LIMIT-REFUND
               PERFORM 3110-LIMIT-REFUND
               PERFORM 3120-SELECT-MARGIN
               PERFORM 3130-DESCRIBE-BRANCH
               PERFORM 3140-ASSESS-OVERRIDE
               PERFORM 3150-COUNT-CYCLE
               PERFORM 3160-LIMIT-COMMISSION
               PERFORM 3170-DESCRIBE-TRAN
               PERFORM 3180-LIMIT-CONSIGN
               PERFORM 3190-SPLIT-REFUND
               PERFORM 3200-ACCRUE-REMIT
               PERFORM 3210-CHECK-POST
               PERFORM 3220-SET-TRAN
               PERFORM 3230-SELECT-FEE-PCT
               PERFORM 3240-FLAG-CONSIGN
               PERFORM 3250-AGE-EFFECTIVE
               PERFORM 3260-PRORATE-REFUND
               PERFORM 3270-AGE-MATURITY
               PERFORM 3280-LIMIT-COMMISSION
               PERFORM 3290-PRORATE-CONSIGN
               PERFORM 3300-ACCRUE-MARGIN
               PERFORM 3310-LIMIT-SHORTAGE
               PERFORM 3320-EDIT-MEMO
               PERFORM 3330-LIMIT-REFUND
               PERFORM 3340-SPLIT-COMMISSION
               PERFORM 3350-FLAG-CONSIGN
               PERFORM 3360-CHECK-MATURITY
               PERFORM 3370-ACCRUE-FEE-PCT
               PERFORM 3380-FLAG-CONSIGN
               PERFORM 3390-FLAG-COMMISSION
               PERFORM 3400-ACCUM-ISSUER
               PERFORM 3410-DESCRIBE-SOURCE
               PERFORM 3420-DESCRIBE-BRANCH
               PERFORM 3430-AGE-LAST-ACT
               PERFORM 3440-SET-CYCLE
               PERFORM 3450-FLAG-ITEM
               PERFORM 3460-SELECT-MARGIN
               PERFORM 3470-SELECT-MARGIN
               PERFORM 3480-CHECK-POST
               PERFORM 3490-LIMIT-REFUND
               PERFORM 3500-ASSESS-SUSPEND
               PERFORM 3510-EDIT-REF-NO
               PERFORM 3520-SPLIT-SHORTAGE
               PERFORM 3530-CHECK-NEXT-DUE
               PERFORM 3540-ACCUM-STATUS
               PERFORM 3550-DESCRIBE-ISSUER
               PERFORM 3560-DESCRIBE-BRANCH
               PERFORM 3570-ACCUM-CHANNEL
               PERFORM 3580-DESCRIBE-ISSUER
               PERFORM 3590-CHECK-EXPIRY
               PERFORM 3600-AGE-LAST-ACT
               PERFORM 3610-ACCRUE-REFUND
               PERFORM 3620-LIMIT-COMMISSION
               PERFORM 3630-SET-DENOM
               PERFORM 3640-ACCUM-SOURCE
               PERFORM 3650-AGE-EFF
               PERFORM 3660-COUNT-ITEM
               PERFORM 3670-PRORATE-FACE
               PERFORM 3680-PRORATE-COMMISSION
               PERFORM 3690-SET-TRAN
               PERFORM 3700-AGE-POST
               PERFORM 3710-LIMIT-SHORTAGE
               PERFORM 3720-SET-DENOM
               PERFORM 3730-ACCUM-STATUS
               PERFORM 3740-ACCRUE-REMIT
               PERFORM 3750-ASSESS-SUSPEND
               PERFORM 3760-SET-BRANCH
               PERFORM 3770-PRORATE-REMIT
               PERFORM 3780-COUNT-ITEM
               PERFORM 3790-LIMIT-COMMISSION
               PERFORM 3800-SPLIT-REFUND
               PERFORM 3810-PRORATE-COMMISSION
               PERFORM 3820-ACCUM-SOURCE
               PERFORM 3830-SPLIT-REMIT
               PERFORM 3840-SET-CYCLE
               PERFORM 3850-ACCUM-DENOM
               PERFORM 3860-LIMIT-CONSIGN
               PERFORM 3870-SPLIT-COMMISSION
               PERFORM 3880-SELECT-RATE
               PERFORM 3890-SET-CYCLE
               PERFORM 3900-FLAG-ITEM
               PERFORM 3910-LIMIT-REMIT
               PERFORM 3920-FLAG-REMIT
               PERFORM 3930-ACCRUE-SHORTAGE
               PERFORM 3940-CHECK-LAST-STMT
               PERFORM 3950-CHECK-EFF
               PERFORM 3960-LIMIT-SHORTAGE
               PERFORM 3970-COUNT-ITEM
               PERFORM 3980-ASSESS-REVERSAL
               PERFORM 3990-CHECK-EXPIRY
               PERFORM 4000-EDIT-OPERATOR
               PERFORM 4010-PRORATE-CONSIGN
               PERFORM 4020-EDIT-REF-NO
               PERFORM 4030-COUNT-RETRY
               PERFORM 4040-ACCRUE-COMMISSION
               PERFORM 4050-SPLIT-REMIT
               PERFORM 7000-CALL-TCCAL
               PERFORM 7020-CALL-TCPRC
               PERFORM 7010-CALL-TCDTE
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
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION FEE ON THE REFUND YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3020-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-COMMISSION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-STOP-FLAG = 'Y'
                   COMPUTE TRCM-COMMISSION-AMT ROUNDED = TRCM-REFUND-YTD
                       * TRCM-FEE-PCT / 200
               WHEN TRCM-ISSUER-CD = 'AF'
                   MOVE 20.00 TO TRCM-COMMISSION-AMT
               WHEN TRCM-ISSUER-CD = 'CJ'
                   MOVE 3.00 TO TRCM-COMMISSION-AMT
               WHEN OTHER
                   COMPUTE TRCM-COMMISSION-AMT ROUNDED = TRCM-REFUND-YTD
                       * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-COMMISSION-AMT > 0 AND TRCM-COMMISSION-AMT < 10.00
               MOVE 10.00 TO TRCM-COMMISSION-AMT
           END-IF
           IF TRCM-COMMISSION-AMT > 100.00
               MOVE 100.00 TO TRCM-COMMISSION-AMT
           END-IF
           ADD TRCM-COMMISSION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-CHANNEL.
           EVALUATE TRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN MATURITY DATE IS PAST
      * CJ -> DU WHEN MATURITY DATE IS PAST
      * DU -> JY WHEN MATURITY DATE IS PAST
      * JY -> RR WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RR' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COMMISSION AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-COMMISSION.
           IF TRCT-COMMISSION-AMT < 0.00
               MOVE 0.00 TO TRCT-COMMISSION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-COMMISSION-AMT > 1000.00
               MOVE 1000.00 TO TRCT-COMMISSION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E674 REF NO REQUIRED
      * E629 REFUND YTD NEGATIVE
      * E067 REFUND YTD OVER 99999
      * E475 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-REF-NO = SPACES
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-YTD < 0
                   MOVE 'E629' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-YTD > 99999.00
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN TRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-CYCLE-CD
                   ADD TRCM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-FACE-AMT ROUNDED =
                   TRCM-REMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-FACE-AMT
           END-IF
           ADD TRCM-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-REFUND.
           IF TRCT-REFUND-AMT < 25.00
               MOVE 25.00 TO TRCT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-REFUND-AMT > 10000.00
               MOVE 10000.00 TO TRCT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3110-LIMIT-REFUND.
           IF TRCT-REFUND-AMT < 0.00
               MOVE 0.00 TO TRCT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-REFUND-AMT > 2500.00
               MOVE 2500.00 TO TRCT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT TIER TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-TIER-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TRCM-REMIT-YTD NOT < WS-REMIT-TIER-LOW(WS-TX)
                   MOVE WS-REMIT-TIER-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT FEE ON THE CONSIGN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3140-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRCM-REMIT-AMT ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-RATE / 200
               WHEN TRCM-SOURCE-CD = 'EG'
                   MOVE 12.00 TO TRCM-REMIT-AMT
               WHEN TRCM-SOURCE-CD = 'GH'
                   MOVE 10.00 TO TRCM-REMIT-AMT
               WHEN OTHER
                   COMPUTE TRCM-REMIT-AMT ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-REMIT-AMT > 0 AND TRCM-REMIT-AMT < 1.00
               MOVE 1.00 TO TRCM-REMIT-AMT
           END-IF
           IF TRCM-REMIT-AMT > 75.00
               MOVE 75.00 TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-COMMISSION.
           IF TRCM-COMMISSION-YTD < 25.00
               MOVE 25.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-COMMISSION-YTD > 1000.00
               MOVE 1000.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONSIGN YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-CONSIGN.
           IF TRCM-CONSIGN-YTD < 100.00
               MOVE 100.00 TO TRCM-CONSIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-CONSIGN-YTD > 5000.00
               MOVE 5000.00 TO TRCM-CONSIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCM-REFUND-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRCM-REFUND-YTD - (WS-WORK-AMT * 2)
           COMPUTE TRCT-COMMISSION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-REMIT.
           IF TRCT-REMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-REMIT-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TRCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SL -> RF WHEN EXPIRY DATE IS PAST
      * RF -> RM WHEN EXPIRY DATE IS PAST
      * RM -> CN WHEN EXPIRY DATE IS PAST
      * CN -> VD WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-TRAN.
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
      * SELECT THE REMIT TIER TIER FOR THE CONSIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-TIER-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TRCM-CONSIGN-AMT NOT < WS-REMIT-TIER-LOW(WS-TX)
                   MOVE WS-REMIT-TIER-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3240-FLAG-CONSIGN.
           IF TRCT-CONSIGN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCT-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       3260-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REFUND-YTD ROUNDED =
                   TRCM-FACE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-REFUND-YTD
           END-IF
           ADD TRCM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-MATURITY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COMMISSION AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3280-LIMIT-COMMISSION.
           IF TRCT-COMMISSION-AMT < 50.00
               MOVE 50.00 TO TRCT-COMMISSION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-COMMISSION-AMT > 1000.00
               MOVE 1000.00 TO TRCT-COMMISSION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       3290-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-CONSIGN-AMT ROUNDED =
                   TRCM-REMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCT-CONSIGN-AMT
           END-IF
           ADD TRCT-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3310-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-AMT < 0.00
               MOVE 0.00 TO TRCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-AMT > 50000.00
               MOVE 50000.00 TO TRCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E667 MEMO REQUIRED
      * E861 REFUND AMT NEGATIVE
      * E524 REFUND AMT OVER 250000
      * E005 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-MEMO = SPACES
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN TRCT-REFUND-AMT < 0
                   MOVE 'E861' TO WS-REJECT-CODE
               WHEN TRCT-REFUND-AMT > 250000.00
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN TRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3330-LIMIT-REFUND.
           IF TRCT-REFUND-AMT < 0.00
               MOVE 0.00 TO TRCT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-REFUND-AMT > 2500.00
               MOVE 2500.00 TO TRCT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TRCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3350-FLAG-CONSIGN.
           IF TRCM-CONSIGN-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-MATURITY-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TRCM-REMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3380-FLAG-CONSIGN.
           IF TRCM-CONSIGN-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3390-FLAG-COMMISSION.
           IF TRCM-COMMISSION-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-COMMISSION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-ISSUER-CD
                   ADD TRCM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-LAST-ACT.
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
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EXPIRY DATE IS PAST
      * HHK -> JCB WHEN EXPIRY DATE IS PAST
      * JCB -> UCB WHEN EXPIRY DATE IS PAST
      * UCB -> XTY WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-CYCLE.
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
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3450-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRCM-FACE-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRCM-FACE-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TRCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-POST-DATE TO DTU-DATE-1
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
      * REFUND YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3490-LIMIT-REFUND.
           IF TRCM-REFUND-YTD < 50.00
               MOVE 50.00 TO TRCM-REFUND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REFUND-YTD > 1000.00
               MOVE 1000.00 TO TRCM-REFUND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3500-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRCM-SHORTAGE-YTD *
                       TRCM-MARGIN / 200
               WHEN TRCM-SOURCE-CD = 'EG'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN TRCM-SOURCE-CD = 'GH'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRCM-SHORTAGE-YTD *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E139 REF NO REQUIRED
      * E875 COMMISSION YTD NEGATIVE
      * E598 COMMISSION YTD OVER 1000000
      * E677 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-REF-NO = SPACES
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD < 0
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD > 1000000.00
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-STATUS-CD
                   ADD TRCT-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3560-DESCRIBE-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EXPIRY-DATE TO DTU-DATE-1
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
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-LAST-ACT.
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
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-REFUND.
           IF TRCM-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REFUND-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-COMMISSION.
           IF TRCM-COMMISSION-YTD < 25.00
               MOVE 25.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-COMMISSION-YTD > 5000.00
               MOVE 5000.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * G -> M WHEN LAST ACT DATE IS PAST
      * M -> N WHEN LAST ACT DATE IS PAST
      * N -> P WHEN LAST ACT DATE IS PAST
      * P -> V WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-DENOM.
           EVALUATE TRCM-DENOM-CD
               WHEN 'G'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-SOURCE-CD
                   ADD TRCM-CONSIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-CONSIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-EFF.
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
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-FACE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-FACE-AMT
           END-IF
           ADD TRCM-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COMMISSION AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-COMMISSION-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCT-COMMISSION-AMT
           END-IF
           ADD TRCT-COMMISSION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SL -> RF WHEN EXPIRY DATE IS PAST
      * RF -> RM WHEN EXPIRY DATE IS PAST
      * RM -> CN WHEN EXPIRY DATE IS PAST
      * CN -> VD WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TRAN.
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
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-COMMISSION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-YTD < 50.00
               MOVE 50.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-YTD > 10000.00
               MOVE 10000.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * G -> M WHEN CLOSE DATE IS PAST
      * M -> N WHEN CLOSE DATE IS PAST
      * N -> P WHEN CLOSE DATE IS PAST
      * P -> V WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-DENOM.
           EVALUATE TRCM-DENOM-CD
               WHEN 'G'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-STATUS-CD
                   ADD TRCM-CONSIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-CONSIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-REMIT.
           IF TRCM-REMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-YTD * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCT-COMMISSION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION FEE ON THE CONSIGN AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3750-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-COMMISSION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCM-COMMISSION-AMT ROUNDED =
                       TRCT-CONSIGN-AMT * TRCM-FEE-PCT / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 10.00 TO TRCM-COMMISSION-AMT
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 20.00 TO TRCM-COMMISSION-AMT
               WHEN OTHER
                   COMPUTE TRCM-COMMISSION-AMT ROUNDED =
                       TRCT-CONSIGN-AMT * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-COMMISSION-AMT > 0 AND TRCM-COMMISSION-AMT < 5.00
               MOVE 5.00 TO TRCM-COMMISSION-AMT
           END-IF
           IF TRCM-COMMISSION-AMT > 35.00
               MOVE 35.00 TO TRCM-COMMISSION-AMT
           END-IF
           ADD TRCM-COMMISSION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EG -> HZ WHEN CLOSE DATE IS PAST
      * HZ -> JF WHEN CLOSE DATE IS PAST
      * JF -> NZ WHEN CLOSE DATE IS PAST
      * NZ -> EG WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HZ' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HZ'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EG' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REMIT-AMT ROUNDED =
                   TRCM-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 3
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-COMMISSION.
           IF TRCM-COMMISSION-AMT < 0.00
               MOVE 0.00 TO TRCM-COMMISSION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-COMMISSION-AMT > 1000.00
               MOVE 1000.00 TO TRCM-COMMISSION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCM-REFUND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-REFUND-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRCM-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONSIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COMMISSION AMT.
      *----------------------------------------------------------------*
       3810-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-COMMISSION-AMT ROUNDED =
                   TRCM-CONSIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-COMMISSION-AMT
           END-IF
           ADD TRCM-COMMISSION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-SOURCE.
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
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRCM-REMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN MATURITY DATE IS PAST
      * HHK -> JCB WHEN MATURITY DATE IS PAST
      * JCB -> UCB WHEN MATURITY DATE IS PAST
      * UCB -> XTY WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-DENOM-CD
                   ADD TRCM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-CONSIGN.
           IF TRCM-CONSIGN-AMT < 100.00
               MOVE 100.00 TO TRCM-CONSIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-CONSIGN-AMT > 10000.00
               MOVE 10000.00 TO TRCM-CONSIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-YTD - (WS-WORK-AMT *
               4)
           COMPUTE TRCM-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-TIER-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-REMIT-TIER-LOW(WS-TX)
                   MOVE WS-REMIT-TIER-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EFFECTIVE DATE IS PAST
      * HHK -> JCB WHEN EFFECTIVE DATE IS PAST
      * JCB -> UCB WHEN EFFECTIVE DATE IS PAST
      * UCB -> XTY WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-CYCLE.
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
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3900-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3910-LIMIT-REMIT.
           IF TRCT-REMIT-AMT < 50.00
               MOVE 50.00 TO TRCT-REMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-REMIT-AMT > 10000.00
               MOVE 10000.00 TO TRCT-REMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3920-FLAG-REMIT.
           IF TRCM-REMIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-REMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-SHORTAGE.
           IF TRCM-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-SHORTAGE-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-LAST-STMT.
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
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-EFF.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO TRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3960-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-AMT < 50.00
               MOVE 50.00 TO TRCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-AMT > 500.00
               MOVE 500.00 TO TRCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 12
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3980-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TRCT-REFUND-AMT ROUNDED = WS-WORK-AMT-2 *
                       TRCM-FEE-PCT / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 10.00 TO TRCT-REFUND-AMT
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 12.00 TO TRCT-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCT-REFUND-AMT ROUNDED = WS-WORK-AMT-2 *
                       TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCT-REFUND-AMT > 0 AND TRCT-REFUND-AMT < 1.00
               MOVE 1.00 TO TRCT-REFUND-AMT
           END-IF
           IF TRCT-REFUND-AMT > 100.00
               MOVE 100.00 TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E238 OPERATOR REQUIRED
      * E570 WORK AMT NEGATIVE
      * E148 WORK AMT OVER 250000
      * E840 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-OPERATOR = SPACES
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E570' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN TRCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-AMT ROUNDED =
                   TRCM-SHORTAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-AMT
           END-IF
           ADD TRCM-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E345 REF NO REQUIRED
      * E747 REFUND AMT NEGATIVE
      * E261 REFUND AMT OVER 1000000
      * E261 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-REF-NO = SPACES
                   MOVE 'E345' TO WS-REJECT-CODE
               WHEN TRCT-REFUND-AMT < 0
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN TRCT-REFUND-AMT > 1000000.00
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN TRCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-COMMISSION.
           IF TRCM-COMMISSION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-COMMISSION-AMT * TRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRCM-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4050-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCM-REMIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRCM-REMIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRCM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TRC FIELDS TO TCCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TCCAL.
           INITIALIZE TCCAL-PARMS
           MOVE TRCM-COMMISSION-YTD
             TO TCCAL-COMMISSION-YTD
           MOVE TRCM-SHORTAGE-YTD
             TO TCCAL-SHORTAGE-YTD
           MOVE TRCM-COMMISSION-AMT
             TO TCCAL-COMMISSION-AMT
           MOVE TRCM-SHORTAGE-AMT
             TO TCCAL-SHORTAGE-AMT
           MOVE TRCM-CONSIGN-AMT
             TO TCCAL-CONSIGN-AMT
           MOVE TRCM-CONSIGN-YTD
             TO TCCAL-CONSIGN-YTD
           MOVE TRCM-MATURITY-DATE
             TO TCCAL-MATURITY-DATE
           MOVE TRCM-LAST-ACT-DATE
             TO TCCAL-LAST-ACT-DATE
           MOVE TRCM-EFFECTIVE-DATE
             TO TCCAL-EFFECTIVE-DATE
           MOVE TRCM-NEXT-DUE-DATE
             TO TCCAL-NEXT-DUE-DATE
           MOVE TRCM-BRANCH-CD
             TO TCCAL-BRANCH-CD
           MOVE TRCM-DENOM-CD
             TO TCCAL-DENOM-CD
           MOVE TRCM-ISSUER-CD
             TO TCCAL-ISSUER-CD
           MOVE TRCM-SOURCE-CD
             TO TCCAL-SOURCE-CD
           MOVE TRCM-EXEMPT-FLAG
             TO TCCAL-EXEMPT-FLAG
           MOVE TRCM-REVIEW-FLAG
             TO TCCAL-REVIEW-FLAG
           MOVE TRCM-STOP-FLAG
             TO TCCAL-STOP-FLAG
           MOVE TRCM-EXCEPT-CNT
             TO TCCAL-EXCEPT-CNT
           MOVE TRCM-ITEM-CNT
             TO TCCAL-ITEM-CNT
           MOVE TRCM-RATE
             TO TCCAL-RATE
           MOVE TRCM-FEE-PCT
             TO TCCAL-FEE-PCT
           MOVE TRCM-NAME
             TO TCCAL-NAME
           MOVE TRCM-ADDR
             TO TCCAL-ADDR
           CALL 'TCCAL' USING TCCAL-PARMS
           EVALUATE TCCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TCCAL-SHORTAGE-YTD
                     TO TRCM-SHORTAGE-YTD
                   MOVE TCCAL-SHORTAGE-AMT
                     TO TRCM-SHORTAGE-AMT
                   MOVE TCCAL-REVIEW-FLAG
                     TO TRCM-REVIEW-FLAG
                   MOVE TCCAL-STOP-FLAG
                     TO TRCM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TCCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE TCPRC-REMIT-YTD
                     TO TRCM-REMIT-YTD
                   MOVE TCPRC-CONSIGN-YTD
                     TO TRCM-CONSIGN-YTD
                   MOVE TCPRC-SHORTAGE-YTD
                     TO TRCM-SHORTAGE-YTD
                   MOVE TCPRC-CONSIGN-AMT
                     TO TRCM-CONSIGN-AMT
                   MOVE TCPRC-NOTICE-SENT-FLAG
                     TO TRCM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TCPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRC FIELDS TO TCDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TCDTE.
           INITIALIZE TCDTE-PARMS
           MOVE TRCM-SHORTAGE-AMT
             TO TCDTE-SHORTAGE-AMT
           MOVE TRCM-COMMISSION-YTD
             TO TCDTE-COMMISSION-YTD
           MOVE TRCM-REFUND-YTD
             TO TCDTE-REFUND-YTD
           MOVE TRCM-SHORTAGE-YTD
             TO TCDTE-SHORTAGE-YTD
           MOVE TRCM-CONSIGN-AMT
             TO TCDTE-CONSIGN-AMT
           MOVE TRCM-REFUND-AMT
             TO TCDTE-REFUND-AMT
           MOVE TRCM-MATURITY-DATE
             TO TCDTE-MATURITY-DATE
           MOVE TRCM-CLOSE-DATE
             TO TCDTE-CLOSE-DATE
           MOVE TRCM-NEXT-DUE-DATE
             TO TCDTE-NEXT-DUE-DATE
           MOVE TRCM-LAST-ACT-DATE
             TO TCDTE-LAST-ACT-DATE
           MOVE TRCM-BRANCH-CD
             TO TCDTE-BRANCH-CD
           MOVE TRCM-ISSUER-CD
             TO TCDTE-ISSUER-CD
           MOVE TRCM-CYCLE-CD
             TO TCDTE-CYCLE-CD
           MOVE TRCM-STATUS-CD
             TO TCDTE-STATUS-CD
           MOVE TRCM-STOP-FLAG
             TO TCDTE-STOP-FLAG
           MOVE TRCM-OVERRIDE-FLAG
             TO TCDTE-OVERRIDE-FLAG
           MOVE TRCM-REVIEW-FLAG
             TO TCDTE-REVIEW-FLAG
           MOVE TRCM-EXCEPT-CNT
             TO TCDTE-EXCEPT-CNT
           MOVE TRCM-ITEM-CNT
             TO TCDTE-ITEM-CNT
           MOVE TRCM-RATE
             TO TCDTE-RATE
           MOVE TRCM-MARGIN
             TO TCDTE-MARGIN
           MOVE TRCM-REF-NO
             TO TCDTE-REF-NO
           MOVE TRCM-CUST-ID
             TO TCDTE-CUST-ID
           CALL 'TCDTE' USING TCDTE-PARMS
           EVALUATE TCDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TCDTE-COMMISSION-YTD
                     TO TRCM-COMMISSION-YTD
                   MOVE TCDTE-SHORTAGE-YTD
                     TO TRCM-SHORTAGE-YTD
                   MOVE TCDTE-CONSIGN-AMT
                     TO TRCM-CONSIGN-AMT
                   MOVE TCDTE-STOP-FLAG
                     TO TRCM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TCDTE' TO ABND-PARAGRAPH
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
           DISPLAY 'TCCSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TCCSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
