      *================================================================*
      * PROGRAM  : FNNSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : FOREIGN BANKNOTES - SETTLEMENT. READS THE FXN       *
      *            TRANSACTIONS, UPDATES THE FXN MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FOREIGN CURRENCY BANKNOTE       *
      *            ORDERS, BUYBACK AND MARGIN                          *
      * INPUT    : FXNMST (KSDS), FXNTRN, BANKCTL                      *
      * OUTPUT   : FXNOUT, FNNSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, FNVAL, FNTIER           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-05-04 EWS  ORIGINAL                                       *
      * 1998-04-19 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-06 PQR  USD FEE SCHEDULE UPDATE INC5158                *
      * 2000-05-06 JLM  PERFORMANCE - MARGIN READ AHEAD INC1689        *
      * 2000-09-02 BHS  BUYBACK CALCULATION CORRECTED PRB3285          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNNSETL.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "FXNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FXNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "FXNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "FXNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "FNNSERPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY FXNMST.
       FD  TRAN-FILE.
       COPY FXNTRN.
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
                                           VALUE 23.000000.
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
      * WS-USD-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-USD-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000524000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000280500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000530500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000111000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000898500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026250000000111000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000884000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031350000000328000'.
       01  WS-USD-SCHED-TBL REDEFINES WS-USD-SCHED-VALUES.
           05  WS-USD-SCHED-ROW OCCURS 8 TIMES.
               10  WS-USD-SCHED-LOW        PIC 9(09)V99.
               10  WS-USD-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUYBACK-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUYBACK-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000175000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000562000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000864500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000869000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000364500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025050000000097000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025150000000120500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025650000000709000'.
       01  WS-BUYBACK-BAND-TBL REDEFINES WS-BUYBACK-BAND-VALUES.
           05  WS-BUYBACK-BAND-ROW OCCURS 8 TIMES.
               10  WS-BUYBACK-BAND-LOW     PIC 9(09)V99.
               10  WS-BUYBACK-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FOREIGN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FOREIGN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000377000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000093500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000600000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000476500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001400000000708000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000548500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000844500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000586000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004550000000424000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014550000000810000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014800000000265000'.
       01  WS-FOREIGN-TIER-TBL REDEFINES WS-FOREIGN-TIER-VALUES.
           05  WS-FOREIGN-TIER-ROW OCCURS 11 TIMES.
               10  WS-FOREIGN-TIER-LOW     PIC 9(09)V99.
               10  WS-FOREIGN-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHIPPING-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHIPPING-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000823000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000546500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000665000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000356500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002100000000100000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000760500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000290500'.
       01  WS-SHIPPING-TIER-TBL REDEFINES WS-SHIPPING-TIER-VALUES.
           05  WS-SHIPPING-TIER-ROW OCCURS 7 TIMES.
               10  WS-SHIPPING-TIER-LOW    PIC 9(09)V99.
               10  WS-SHIPPING-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  FNNSETL  FOREIGN BANKNOTES'.
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
       COPY FNVALP.
       COPY FNTIERP.

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
               MOVE 'FXNMST' TO ABND-FILE-NAME
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
           MOVE FXNT-KEY TO FXNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-TRAN
               PERFORM 3010-COUNT-NOTICE
               PERFORM 3020-LIMIT-MARGIN
               PERFORM 3030-SELECT-MARGIN
               PERFORM 3040-AGE-OPEN
               PERFORM 3050-ASSESS-REVERSAL
               PERFORM 3060-ASSESS-EXEMPT
               PERFORM 3070-COUNT-CYCLE
               PERFORM 3080-LIMIT-FOREIGN
               PERFORM 3090-SPLIT-FOREIGN
               PERFORM 3100-AGE-LAST-MAINT
               PERFORM 3110-SPLIT-USD
               PERFORM 3120-DESCRIBE-BRANCH
               PERFORM 3130-SPLIT-SHIPPING
               PERFORM 3140-SPLIT-USD
               PERFORM 3150-FLAG-USD
               PERFORM 3160-PRORATE-MARGIN
               PERFORM 3170-ASSESS-PREFERRED
               PERFORM 3180-SET-ORDER-TYPE
               PERFORM 3190-SPLIT-ITEM
               PERFORM 3200-ACCRUE-SHIPPING
               PERFORM 3210-SPLIT-USD
               PERFORM 3220-FLAG-BUYBACK
               PERFORM 3230-SET-CHANNEL
               PERFORM 3240-PRORATE-USD
               PERFORM 3250-COUNT-SEQ
               PERFORM 3260-PRORATE-SHIPPING
               PERFORM 3270-CHECK-CLOSE
               PERFORM 3280-ACCRUE-FOREIGN
               PERFORM 3290-SET-TYPE
               PERFORM 3300-CHECK-CLOSE
               PERFORM 3310-ACCRUE-MARGIN
               PERFORM 3320-COUNT-PAYMENT
               PERFORM 3330-AGE-EFF
               PERFORM 3340-AGE-OPEN
               PERFORM 3350-SPLIT-SHIPPING
               PERFORM 3360-AGE-LAST-PAY
               PERFORM 3370-SET-TRAN
               PERFORM 3380-EDIT-CONTACT
               PERFORM 3390-AGE-OPEN
               PERFORM 3400-AGE-LAST-MAINT
               PERFORM 3410-SPLIT-USD
               PERFORM 3420-SET-TYPE
               PERFORM 3430-AGE-LAST-MAINT
               PERFORM 3440-COUNT-NOTICE
               PERFORM 3450-SPLIT-MARGIN
               PERFORM 3460-ACCUM-STATUS
               PERFORM 3470-COUNT-NOTICE
               PERFORM 3480-AGE-LAST-PAY
               PERFORM 3490-PRORATE-MARGIN
               PERFORM 3500-SPLIT-BUYBACK
               PERFORM 3510-PRORATE-FOREIGN
               PERFORM 3520-EDIT-CUST-ID
               PERFORM 3530-SELECT-PENALTY-PCT
               PERFORM 3540-CHECK-NOTICE
               PERFORM 3550-LIMIT-MARGIN
               PERFORM 3560-ACCUM-CURRENCY
               PERFORM 3570-ACCUM-CHANNEL
               PERFORM 3580-COUNT-NOTICE
               PERFORM 3590-SELECT-RATE
               PERFORM 3600-ACCRUE-PENALTY-PCT
               PERFORM 3610-LIMIT-FOREIGN
               PERFORM 3620-CHECK-OPEN
               PERFORM 3630-FLAG-ITEM
               PERFORM 3640-PRORATE-SHIPPING
               PERFORM 3650-DESCRIBE-ORDER-TYPE
               PERFORM 3660-FLAG-MARGIN
               PERFORM 3670-ACCUM-CHANNEL
               PERFORM 3680-FLAG-MARGIN
               PERFORM 3690-SELECT-RATE
               PERFORM 3700-PRORATE-BUYBACK
               PERFORM 3710-ACCRUE-SHIPPING
               PERFORM 3720-FLAG-FOREIGN
               PERFORM 3730-FLAG-USD
               PERFORM 3740-COUNT-PAYMENT
               PERFORM 3750-CHECK-OPEN
               PERFORM 3760-SET-SOURCE
               PERFORM 3770-SET-SOURCE
               PERFORM 3780-PRORATE-BUYBACK
               PERFORM 3790-PRORATE-USD
               PERFORM 3800-DESCRIBE-TYPE
               PERFORM 3810-COUNT-PAYMENT
               PERFORM 3820-AGE-CLOSE
               PERFORM 3830-CHECK-POST
               PERFORM 3840-ACCRUE-BUYBACK
               PERFORM 3850-SET-BRANCH
               PERFORM 3860-ACCUM-ORDER-TYPE
               PERFORM 3870-ASSESS-OVERRIDE
               PERFORM 3880-CHECK-EFF
               PERFORM 3890-CHECK-LAST-ACT
               PERFORM 3900-LIMIT-SHIPPING
               PERFORM 3910-SPLIT-BUYBACK
               PERFORM 3920-ACCUM-TYPE
               PERFORM 3930-COUNT-CYCLE
               PERFORM 3940-ASSESS-PREFERRED
               PERFORM 3950-LIMIT-BUYBACK
               PERFORM 3960-LIMIT-BUYBACK
               PERFORM 3970-SELECT-PENALTY-PCT
               PERFORM 3980-AGE-EFFECTIVE
               PERFORM 3990-AGE-CLOSE
               PERFORM 4000-CHECK-LAST-PAY
               PERFORM 4010-SELECT-MARGIN
               PERFORM 4020-PRORATE-BUYBACK
               PERFORM 4030-EDIT-REF-NO
               PERFORM 4040-AGE-MATURITY
               PERFORM 4050-AGE-OPEN
               PERFORM 4060-SET-CHANNEL
               PERFORM 4070-SELECT-MARGIN
               PERFORM 4080-DESCRIBE-TRAN
               PERFORM 4090-SPLIT-BUYBACK
               PERFORM 4100-COUNT-EXCEPT
               PERFORM 4110-LIMIT-MARGIN
               PERFORM 4120-LIMIT-BUYBACK
               PERFORM 4130-ACCUM-SOURCE
               PERFORM 4140-DESCRIBE-CURRENCY
               PERFORM 4150-PRORATE-FOREIGN
               PERFORM 4160-AGE-LAST-MAINT
               PERFORM 4170-PRORATE-USD
               PERFORM 4180-CHECK-LAST-MAINT
               PERFORM 4190-EDIT-ADDR
               PERFORM 4200-EDIT-CUST-ID
               PERFORM 4210-EDIT-REJECT
               PERFORM 4220-CHECK-POST
               PERFORM 4230-COUNT-CYCLE
               PERFORM 4240-SELECT-RATE
               PERFORM 4250-SET-SOURCE
               PERFORM 4260-PRORATE-FOREIGN
               PERFORM 4270-DESCRIBE-TRAN
               PERFORM 4280-ACCUM-TRAN
               PERFORM 4290-AGE-NOTICE
               PERFORM 4300-ASSESS-EXEMPT
               PERFORM 4310-PRORATE-SHIPPING
               PERFORM 4320-PRORATE-SHIPPING
               PERFORM 4330-SELECT-RATE
               PERFORM 4340-ACCUM-CHANNEL
               PERFORM 4350-CHECK-LAST-ACT
               PERFORM 4360-AGE-EXPIRY
               PERFORM 4370-SPLIT-BUYBACK
               PERFORM 4380-COUNT-NOTICE
               PERFORM 4390-CHECK-POST
               PERFORM 4400-ACCRUE-MARGIN
               PERFORM 4410-AGE-MATURITY
               PERFORM 4420-ACCRUE-SHIPPING
               PERFORM 4430-SPLIT-FOREIGN
               PERFORM 4440-DESCRIBE-TYPE
               PERFORM 4450-SPLIT-USD
               PERFORM 4460-CHECK-MATURITY
               PERFORM 4470-ACCUM-SOURCE
               PERFORM 4480-ASSESS-REVERSAL
               PERFORM 4490-PRORATE-SHIPPING
               PERFORM 4500-FLAG-USD
               PERFORM 4510-ACCRUE-USD
               PERFORM 4520-AGE-LAST-PAY
               PERFORM 4530-ASSESS-AUTO-RENEW
               PERFORM 4540-ACCRUE-SHIPPING
               PERFORM 4550-PRORATE-MARGIN
               PERFORM 4560-SPLIT-SHIPPING
               PERFORM 4570-CHECK-POST
               PERFORM 4580-SET-STATUS
               PERFORM 4590-ASSESS-OVERRIDE
               PERFORM 4600-EDIT-DESC
               PERFORM 4610-CHECK-POST
               PERFORM 4620-LIMIT-SHIPPING
               PERFORM 4630-FLAG-FOREIGN
               PERFORM 4640-CHECK-LAST-PAY
               PERFORM 4650-LIMIT-MARGIN
               PERFORM 4660-CHECK-LAST-ACT
               PERFORM 4670-AGE-LAST-MAINT
               PERFORM 4680-LIMIT-USD
               PERFORM 4690-COUNT-NOTICE
               PERFORM 4700-LIMIT-USD
               PERFORM 4710-ACCRUE-FOREIGN
               PERFORM 4720-SELECT-RATE
               PERFORM 4730-LIMIT-FOREIGN
               PERFORM 4740-DESCRIBE-SOURCE
               PERFORM 4750-CHECK-EFF
               PERFORM 4760-ACCUM-ORDER-TYPE
               PERFORM 4770-PRORATE-MARGIN
               PERFORM 4780-EDIT-OPERATOR
               PERFORM 4790-AGE-POST
               PERFORM 4800-EDIT-OPERATOR
               PERFORM 4810-SELECT-PENALTY-PCT
               PERFORM 4820-AGE-LAST-MAINT
               PERFORM 4830-COUNT-SEQ
               PERFORM 4840-ASSESS-AUTO-RENEW
               PERFORM 4850-SPLIT-BUYBACK
               PERFORM 4860-SELECT-MARGIN
               PERFORM 4870-EDIT-CONTACT
               PERFORM 4880-ACCUM-TRAN
               PERFORM 4890-FLAG-BUYBACK
               PERFORM 7030-CALL-FNVAL
               PERFORM 7000-CALL-FNTIER
               REWRITE FXN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'FXNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM FXN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN LAST MAINT DATE IS PAST
      * BB -> SH WHEN LAST MAINT DATE IS PAST
      * SH -> CN WHEN LAST MAINT DATE IS PAST
      * CN -> OR WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3020-LIMIT-MARGIN.
           IF FXNM-MARGIN-AMT < 25.00
               MOVE 25.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FXNM-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE USD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-USD-YTD NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-USD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING FEE ON THE MARGIN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3050-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 20.00 TO FXNM-SHIPPING-AMT
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 7.00 TO FXNM-SHIPPING-AMT
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-AMT ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-SHIPPING-AMT > 0 AND FXNM-SHIPPING-AMT < 5.00
               MOVE 5.00 TO FXNM-SHIPPING-AMT
           END-IF
           IF FXNM-SHIPPING-AMT > 35.00
               MOVE 35.00 TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE BUYBACK YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-BUYBACK-YTD
                       * FXNM-PENALTY-PCT / 200
               WHEN FXNT-TRAN-CD = 'OR'
                   MOVE 15.00 TO FXNM-SHIPPING-YTD
               WHEN FXNT-TRAN-CD = 'BB'
                   MOVE 5.00 TO FXNM-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-BUYBACK-YTD
                       * FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-SHIPPING-YTD > 0 AND FXNM-SHIPPING-YTD < 10.00
               MOVE 10.00 TO FXNM-SHIPPING-YTD
           END-IF
           IF FXNM-SHIPPING-YTD > 75.00
               MOVE 75.00 TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-FOREIGN.
           IF FXNT-FOREIGN-AMT < 25.00
               MOVE 25.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-FOREIGN-AMT > 10000.00
               MOVE 10000.00 TO FXNT-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNT-FOREIGN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNT-FOREIGN-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT USD AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNT-USD-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNT-USD-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BBP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ENL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EZB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KHV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TDN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE FXNM-SHIPPING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT USD AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNM-USD-AMT / 4
           COMPUTE WS-WORK-AMT-2 = FXNM-USD-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNM-FOREIGN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3150-FLAG-USD.
           IF FXNM-USD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-MARGIN-YTD ROUNDED =
                   FXNT-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-MARGIN-YTD
           END-IF
           ADD FXNM-MARGIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK YTD FEE ON THE MARGIN AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3170-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNM-BUYBACK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNM-BUYBACK-YTD ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 7.00 TO FXNM-BUYBACK-YTD
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 15.00 TO FXNM-BUYBACK-YTD
               WHEN OTHER
                   COMPUTE FXNM-BUYBACK-YTD ROUNDED = FXNT-MARGIN-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-BUYBACK-YTD > 0 AND FXNM-BUYBACK-YTD < 5.00
               MOVE 5.00 TO FXNM-BUYBACK-YTD
           END-IF
           IF FXNM-BUYBACK-YTD > 100.00
               MOVE 100.00 TO FXNM-BUYBACK-YTD
           END-IF
           ADD FXNM-BUYBACK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ORDER TYPE CD:
      * DTU -> KZV WHEN LAST ACT DATE IS PAST
      * KZV -> PHV WHEN LAST ACT DATE IS PAST
      * PHV -> PZZ WHEN LAST ACT DATE IS PAST
      * PZZ -> WMC WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KZV'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PHV' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PHV'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PZZ' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PZZ'
                   IF FXNM-LAST-ACT-DATE > 0 AND FXNM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WMC' TO FXNM-ORDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-AMT * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO FXNM-SHIPPING-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT USD AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNT-USD-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-USD-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-BUYBACK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3220-FLAG-BUYBACK.
           IF FXNT-BUYBACK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO USD YTD.
      *----------------------------------------------------------------*
       3240-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-YTD ROUNDED =
                   FXNM-BUYBACK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-USD-YTD
           END-IF
           ADD FXNM-USD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-SEQ.
           IF FXNT-SEQ < 9999999
               ADD 1 TO FXNT-SEQ
           END-IF
           IF FXNT-SEQ NOT < 6
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       3260-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   FXNM-BUYBACK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * FXNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO FXNM-SHIPPING-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN MATURITY DATE IS PAST
      * FFU -> JNR WHEN MATURITY DATE IS PAST
      * JNR -> UGS WHEN MATURITY DATE IS PAST
      * UGS -> XUM WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARGIN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-MARGIN.
           IF FXNM-MARGIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-MARGIN-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FXNM-SHIPPING-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-SHIPPING-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> BB WHEN POST DATE IS PAST
      * BB -> SH WHEN POST DATE IS PAST
      * SH -> CN WHEN POST DATE IS PAST
      * CN -> OR WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BB'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OR' TO FXNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E163 CONTACT REQUIRED
      * E745 MARGIN AMT NEGATIVE
      * E841 MARGIN AMT OVER 250000
      * E047 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CONTACT = SPACES
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN FXNT-MARGIN-AMT < 0
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN FXNT-MARGIN-AMT > 250000.00
                   MOVE 'E841' TO WS-REJECT-CODE
               WHEN FXNM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-MARGIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT USD YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNM-USD-YTD / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-USD-YTD - (WS-WORK-AMT * 3)
           COMPUTE FXNM-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CFP -> FFU WHEN EFF DATE IS PAST
      * FFU -> JNR WHEN EFF DATE IS PAST
      * JNR -> UGS WHEN EFF DATE IS PAST
      * UGS -> XUM WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FFU' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FFU'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNR' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JNR'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGS' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGS'
                   IF FXNT-EFF-DATE > 0 AND FXNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUM' TO FXNM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-MARGIN.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE FXNT-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-STATUS-CD
                   ADD FXNM-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-MARGIN-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-MARGIN-AMT
           END-IF
           ADD FXNM-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNM-BUYBACK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-BUYBACK-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-SHIPPING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FOREIGN YTD.
      *----------------------------------------------------------------*
       3510-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-YTD ROUNDED =
                   FXNM-FOREIGN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E312 CUST ID REQUIRED
      * E259 FOREIGN AMT NEGATIVE
      * E230 FOREIGN AMT OVER 25000
      * E740 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E312' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT < 0
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN FXNM-FOREIGN-AMT > 25000.00
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF FXNM-USD-AMT NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-MARGIN.
           IF FXNT-MARGIN-AMT < 25.00
               MOVE 25.00 TO FXNT-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-MARGIN-AMT > 1000.00
               MOVE 1000.00 TO FXNT-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-CURRENCY-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD FXNT-FOREIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-FOREIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING TIER TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-TIER-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNT-USD-AMT NOT < WS-SHIPPING-TIER-LOW(WS-TX)
                   MOVE WS-SHIPPING-TIER-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO FXNT-BUYBACK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3610-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-AMT < 25.00
               MOVE 25.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-AMT > 2500.00
               MOVE 2500.00 TO FXNM-FOREIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3630-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING AMT.
      *----------------------------------------------------------------*
       3640-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-AMT ROUNDED =
                   FXNM-USD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-AMT
           END-IF
           ADD FXNM-SHIPPING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE ORDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-ORDER-TYPE.
           EVALUATE FXNM-ORDER-TYPE-CD
               WHEN 'DTU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KZV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PHV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PZZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WMC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XWF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MARGIN YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3660-FLAG-MARGIN.
           IF FXNM-MARGIN-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-MARGIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD FXNM-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3680-FLAG-MARGIN.
           IF FXNT-MARGIN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNT-MARGIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-USD-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-BUYBACK-AMT ROUNDED =
                   FXNT-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * FXNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3720-FLAG-FOREIGN.
           IF FXNM-FOREIGN-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE FXNM-FOREIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-USD.
           IF FXNT-USD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNT-USD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN CLOSE DATE IS PAST
      * FS -> HL WHEN CLOSE DATE IS PAST
      * HL -> HS WHEN CLOSE DATE IS PAST
      * HS -> NW WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN LAST PAY DATE IS PAST
      * FS -> HL WHEN LAST PAY DATE IS PAST
      * HL -> HS WHEN LAST PAY DATE IS PAST
      * HS -> NW WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       3780-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-BUYBACK-AMT ROUNDED =
                   FXNM-BUYBACK-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-PAYMENT.
           IF FXNM-PAYMENT-CNT < 9999999
               ADD 1 TO FXNM-PAYMENT-CNT
           END-IF
           IF FXNM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO FXNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-MARGIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUYBACK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-BUYBACK.
           IF FXNT-BUYBACK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-BUYBACK-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNM-BUYBACK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * APZ -> BBP WHEN NOTICE DATE IS PAST
      * BBP -> ENL WHEN NOTICE DATE IS PAST
      * ENL -> EZB WHEN NOTICE DATE IS PAST
      * EZB -> KHV WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-BRANCH.
           EVALUATE FXNM-BRANCH-CD
               WHEN 'APZ'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BBP' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BBP'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ENL' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ENL'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EZB' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EZB'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHV' TO FXNM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNT-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE BUYBACK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3870-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-REVIEW-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNT-BUYBACK-AMT
                       * FXNM-PENALTY-PCT / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 15.00 TO FXNM-SHIPPING-YTD
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 15.00 TO FXNM-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNT-BUYBACK-AMT
                       * FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-SHIPPING-YTD > 0 AND FXNM-SHIPPING-YTD < 10.00
               MOVE 10.00 TO FXNM-SHIPPING-YTD
           END-IF
           IF FXNM-SHIPPING-YTD > 250.00
               MOVE 250.00 TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-SHIPPING.
           IF FXNM-SHIPPING-AMT < 0.00
               MOVE 0.00 TO FXNM-SHIPPING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-SHIPPING-AMT > 2500.00
               MOVE 2500.00 TO FXNM-SHIPPING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNM-BUYBACK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = FXNM-BUYBACK-AMT - (WS-WORK-AMT * 2)
           COMPUTE FXNT-FOREIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-TYPE-CD
                   ADD FXNM-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO FXNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN FEE ON THE BUYBACK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3940-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO FXNT-MARGIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE FXNT-MARGIN-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-ORDER-TYPE-CD = 'DTU'
                   MOVE 5.00 TO FXNT-MARGIN-AMT
               WHEN FXNM-ORDER-TYPE-CD = 'KZV'
                   MOVE 12.00 TO FXNT-MARGIN-AMT
               WHEN OTHER
                   COMPUTE FXNT-MARGIN-AMT ROUNDED = FXNM-BUYBACK-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNT-MARGIN-AMT > 0 AND FXNT-MARGIN-AMT < 5.00
               MOVE 5.00 TO FXNT-MARGIN-AMT
           END-IF
           IF FXNT-MARGIN-AMT > 25.00
               MOVE 25.00 TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BUYBACK AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-BUYBACK.
           IF FXNT-BUYBACK-AMT < 50.00
               MOVE 50.00 TO FXNT-BUYBACK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-BUYBACK-AMT > 10000.00
               MOVE 10000.00 TO FXNT-BUYBACK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 0.00
               MOVE 0.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 5000.00
               MOVE 5000.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING TIER TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNM-SHIPPING-AMT NOT < WS-SHIPPING-TIER-LOW(WS-TX)
                   MOVE WS-SHIPPING-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EFFECTIVE-DATE > 0 AND FXNM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-CLOSE-DATE > 0 AND FXNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FOREIGN TIER TIER FOR THE USD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FOREIGN-TIER-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF FXNM-USD-YTD NOT < WS-FOREIGN-TIER-LOW(WS-TX)
                   MOVE WS-FOREIGN-TIER-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MARGIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUYBACK AMT.
      *----------------------------------------------------------------*
       4020-PRORATE-BUYBACK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-BUYBACK-AMT ROUNDED =
                   FXNT-MARGIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNT-BUYBACK-AMT
           END-IF
           ADD FXNT-BUYBACK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E505 REF NO REQUIRED
      * E182 USD AMT NEGATIVE
      * E433 USD AMT OVER 25000
      * E475 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-REF-NO = SPACES
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN FXNT-USD-AMT < 0
                   MOVE 'E182' TO WS-REJECT-CODE
               WHEN FXNT-USD-AMT > 25000.00
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN FXNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-CHANNEL.
           EVALUATE FXNT-CHANNEL-CD
               WHEN 'BRN'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF FXNM-OPEN-DATE > 0 AND FXNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO FXNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE MARGIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-MARGIN-YTD NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNT-BUYBACK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-BUYBACK-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-EXCEPT.
           IF FXNM-EXCEPT-CNT < 9999999
               ADD 1 TO FXNM-EXCEPT-CNT
           END-IF
           IF FXNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4110-LIMIT-MARGIN.
           IF FXNM-MARGIN-YTD < 25.00
               MOVE 25.00 TO FXNM-MARGIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-MARGIN-YTD > 500.00
               MOVE 500.00 TO FXNM-MARGIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4120-LIMIT-BUYBACK.
           IF FXNM-BUYBACK-YTD < 50.00
               MOVE 50.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-BUYBACK-YTD > 2500.00
               MOVE 2500.00 TO FXNM-BUYBACK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUYBACK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-BUYBACK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-CURRENCY.
           EVALUATE FXNM-CURRENCY-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUYBACK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-FOREIGN-AMT ROUNDED =
                   FXNM-BUYBACK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-FOREIGN-AMT
           END-IF
           ADD FXNT-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHIPPING YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO USD AMT.
      *----------------------------------------------------------------*
       4170-PRORATE-USD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-USD-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-USD-AMT
           END-IF
           ADD FXNM-USD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E381 ADDR REQUIRED
      * E873 SHIPPING YTD NEGATIVE
      * E585 SHIPPING YTD OVER 25000
      * E983 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-ADDR = SPACES
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD < 0
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN FXNM-SHIPPING-YTD > 25000.00
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E398 CUST ID REQUIRED
      * E257 WORK AMT 2 NEGATIVE
      * E625 WORK AMT 2 OVER 25000
      * E779 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CUST-ID = SPACES
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E779' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E976 REJECT CD REQUIRED
      * E088 USD AMT NEGATIVE
      * E713 USD AMT OVER 99999
      * E389 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-REJECT-CD = SPACES
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN FXNM-USD-AMT < 0
                   MOVE 'E088' TO WS-REJECT-CODE
               WHEN FXNM-USD-AMT > 99999.00
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN FXNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E389' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4220-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4230-COUNT-CYCLE.
           IF FXNM-CYCLE-CNT < 9999999
               ADD 1 TO FXNM-CYCLE-CNT
           END-IF
           IF FXNM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-SHIPPING-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DG -> FS WHEN NOTICE DATE IS PAST
      * FS -> HL WHEN NOTICE DATE IS PAST
      * HL -> HS WHEN NOTICE DATE IS PAST
      * HS -> NW WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4250-SET-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FS'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HS' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HS'
                   IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NW' TO FXNM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FOREIGN AMT.
      *----------------------------------------------------------------*
       4260-PRORATE-FOREIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-FOREIGN-AMT ROUNDED =
                   FXNT-FOREIGN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-FOREIGN-AMT
           END-IF
           ADD FXNM-FOREIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4270-DESCRIBE-TRAN.
           EVALUATE FXNT-TRAN-CD
               WHEN 'OR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNT-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF FXNM-NOTICE-DATE > 0 AND FXNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPPING YTD FEE ON THE MARGIN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4300-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO FXNM-SHIPPING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-MARGIN / 200
               WHEN FXNT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO FXNM-SHIPPING-YTD
               WHEN FXNT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO FXNM-SHIPPING-YTD
               WHEN OTHER
                   COMPUTE FXNM-SHIPPING-YTD ROUNDED = FXNM-MARGIN-AMT *
                       FXNM-MARGIN / 100
           END-EVALUATE
           IF FXNM-SHIPPING-YTD > 0 AND FXNM-SHIPPING-YTD < 5.00
               MOVE 5.00 TO FXNM-SHIPPING-YTD
           END-IF
           IF FXNM-SHIPPING-YTD > 50.00
               MOVE 50.00 TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE USD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       4310-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   FXNM-USD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       4320-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   FXNM-FOREIGN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE USD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-USD-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FOREIGN YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-CHANNEL-CD
                   ADD FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-FOREIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4360-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-EXPIRY-DATE > 0 AND FXNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-USD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BUYBACK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4370-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = FXNM-BUYBACK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-BUYBACK-AMT - (WS-WORK-AMT * 12)
           COMPUTE FXNM-USD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4380-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * FXNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO FXNM-USD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-MATURITY-DATE > 0 AND FXNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-BUYBACK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4420-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNM-USD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FOREIGN YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-FOREIGN.
           COMPUTE WS-WORK-AMT = FXNM-FOREIGN-YTD / 12
           COMPUTE WS-WORK-AMT-2 = FXNM-FOREIGN-YTD - (WS-WORK-AMT * 12)
           COMPUTE FXNM-MARGIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4440-DESCRIBE-TYPE.
           EVALUATE FXNM-TYPE-CD
               WHEN 'CFP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FFU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JNR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UGS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XUM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT USD AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-USD.
           COMPUTE WS-WORK-AMT = FXNT-USD-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNT-USD-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4460-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARGIN AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4470-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-SOURCE-CD
                   ADD FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNT-MARGIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FOREIGN YTD FEE ON THE USD YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4480-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN FXNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO FXNM-FOREIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-PREFERRED-FLAG = 'Y'
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-USD-YTD *
                       FXNM-RATE / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 5.00 TO FXNM-FOREIGN-YTD
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 3.00 TO FXNM-FOREIGN-YTD
               WHEN OTHER
                   COMPUTE FXNM-FOREIGN-YTD ROUNDED = FXNM-USD-YTD *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-FOREIGN-YTD > 0 AND FXNM-FOREIGN-YTD < 5.00
               MOVE 5.00 TO FXNM-FOREIGN-YTD
           END-IF
           IF FXNM-FOREIGN-YTD > 250.00
               MOVE 250.00 TO FXNM-FOREIGN-YTD
           END-IF
           ADD FXNM-FOREIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE USD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPPING YTD.
      *----------------------------------------------------------------*
       4490-PRORATE-SHIPPING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-SHIPPING-YTD ROUNDED =
                   FXNM-USD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-SHIPPING-YTD
           END-IF
           ADD FXNM-SHIPPING-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * USD YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4500-FLAG-USD.
           IF FXNM-USD-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-USD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON USD AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-USD.
           IF FXNT-USD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNT-USD-AMT * FXNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO FXNM-FOREIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-SHIPPING-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MARGIN YTD FEE ON THE FOREIGN YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4530-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO FXNM-MARGIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE FXNM-MARGIN-YTD ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-TYPE-CD = 'CFP'
                   MOVE 7.00 TO FXNM-MARGIN-YTD
               WHEN FXNM-TYPE-CD = 'FFU'
                   MOVE 3.00 TO FXNM-MARGIN-YTD
               WHEN OTHER
                   COMPUTE FXNM-MARGIN-YTD ROUNDED = FXNM-FOREIGN-YTD *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF FXNM-MARGIN-YTD > 0 AND FXNM-MARGIN-YTD < 2.00
               MOVE 2.00 TO FXNM-MARGIN-YTD
           END-IF
           IF FXNM-MARGIN-YTD > 35.00
               MOVE 35.00 TO FXNM-MARGIN-YTD
           END-IF
           ADD FXNM-MARGIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPPING YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4540-ACCRUE-SHIPPING.
           IF FXNM-SHIPPING-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-SHIPPING-YTD * FXNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO FXNT-FOREIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN AMT.
      *----------------------------------------------------------------*
       4550-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNT-MARGIN-AMT ROUNDED =
                   FXNM-FOREIGN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNT-MARGIN-AMT
           END-IF
           ADD FXNT-MARGIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT SHIPPING AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-SHIPPING.
           COMPUTE WS-WORK-AMT = FXNM-SHIPPING-AMT / 3
           COMPUTE WS-WORK-AMT-2 = FXNM-SHIPPING-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-MARGIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4570-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CXH -> ESS WHEN LAST PAY DATE IS PAST
      * ESS -> XDE WHEN LAST PAY DATE IS PAST
      * XDE -> XFD WHEN LAST PAY DATE IS PAST
      * XFD -> CXH WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4580-SET-STATUS.
           EVALUATE FXNM-STATUS-CD
               WHEN 'CXH'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ESS' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ESS'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XDE' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XDE'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XFD' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XFD'
                   IF FXNM-LAST-PAY-DATE > 0 AND FXNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CXH' TO FXNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUYBACK FEE ON THE USD AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4590-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN FXNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO FXNM-BUYBACK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-RATE / 200
               WHEN FXNM-STATUS-CD = 'CXH'
                   MOVE 15.00 TO FXNM-BUYBACK-AMT
               WHEN FXNM-STATUS-CD = 'ESS'
                   MOVE 10.00 TO FXNM-BUYBACK-AMT
               WHEN OTHER
                   COMPUTE FXNM-BUYBACK-AMT ROUNDED = FXNM-USD-AMT *
                       FXNM-RATE / 100
           END-EVALUATE
           IF FXNM-BUYBACK-AMT > 0 AND FXNM-BUYBACK-AMT < 2.00
               MOVE 2.00 TO FXNM-BUYBACK-AMT
           END-IF
           IF FXNM-BUYBACK-AMT > 100.00
               MOVE 100.00 TO FXNM-BUYBACK-AMT
           END-IF
           ADD FXNM-BUYBACK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E352 DESC REQUIRED
      * E624 WORK AMT NEGATIVE
      * E109 WORK AMT OVER 5000
      * E114 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4600-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-DESC = SPACES
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN FXNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4610-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO FXNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPPING AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4620-LIMIT-SHIPPING.
           IF FXNM-SHIPPING-AMT < 50.00
               MOVE 50.00 TO FXNM-SHIPPING-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-SHIPPING-AMT > 50000.00
               MOVE 50000.00 TO FXNM-SHIPPING-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4630-FLAG-FOREIGN.
           IF FXNM-FOREIGN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE FXNM-FOREIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4640-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARGIN AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4650-LIMIT-MARGIN.
           IF FXNT-MARGIN-AMT < 10.00
               MOVE 10.00 TO FXNT-MARGIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNT-MARGIN-AMT > 2500.00
               MOVE 2500.00 TO FXNT-MARGIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4660-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO FXNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4670-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
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
      * USD YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4680-LIMIT-USD.
           IF FXNM-USD-YTD < 100.00
               MOVE 100.00 TO FXNM-USD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-YTD > 2500.00
               MOVE 2500.00 TO FXNM-USD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4690-COUNT-NOTICE.
           IF FXNM-NOTICE-CNT < 9999999
               ADD 1 TO FXNM-NOTICE-CNT
           END-IF
           IF FXNM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO FXNM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * USD AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4700-LIMIT-USD.
           IF FXNM-USD-AMT < 50.00
               MOVE 50.00 TO FXNM-USD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-USD-AMT > 50000.00
               MOVE 50000.00 TO FXNM-USD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FOREIGN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4710-ACCRUE-FOREIGN.
           IF FXNM-FOREIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   FXNM-FOREIGN-AMT * FXNM-RATE / 100 / 365
               ADD WS-WORK-AMT TO FXNM-SHIPPING-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUYBACK BAND TIER FOR THE SHIPPING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4720-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUYBACK-BAND-RATE(1) TO FXNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-SHIPPING-AMT NOT < WS-BUYBACK-BAND-LOW(WS-TX)
                   MOVE WS-BUYBACK-BAND-RATE(WS-TX) TO FXNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FOREIGN YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4730-LIMIT-FOREIGN.
           IF FXNM-FOREIGN-YTD < 0.00
               MOVE 0.00 TO FXNM-FOREIGN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF FXNM-FOREIGN-YTD > 5000.00
               MOVE 5000.00 TO FXNM-FOREIGN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4740-DESCRIBE-SOURCE.
           EVALUATE FXNM-SOURCE-CD
               WHEN 'DG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4750-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF FXNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO FXNM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD AMT BY ORDER TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4760-ACCUM-ORDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNM-ORDER-TYPE-CD
                   ADD FXNM-USD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNM-ORDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FOREIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MARGIN YTD.
      *----------------------------------------------------------------*
       4770-PRORATE-MARGIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE FXNM-MARGIN-YTD ROUNDED =
                   FXNT-FOREIGN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO FXNM-MARGIN-YTD
           END-IF
           ADD FXNM-MARGIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E383 OPERATOR REQUIRED
      * E222 MARGIN YTD NEGATIVE
      * E191 MARGIN YTD OVER 99999
      * E070 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4780-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD < 0
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN FXNM-MARGIN-YTD > 99999.00
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN FXNM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4790-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF FXNT-POST-DATE > 0 AND FXNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNM-BUYBACK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E043 OPERATOR REQUIRED
      * E694 USD YTD NEGATIVE
      * E493 USD YTD OVER 5000
      * E815 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4800-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNT-OPERATOR = SPACES
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD < 0
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN FXNM-USD-YTD > 5000.00
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN FXNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHIPPING TIER TIER FOR THE BUYBACK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4810-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHIPPING-TIER-RATE(1) TO FXNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF FXNM-BUYBACK-YTD NOT < WS-SHIPPING-TIER-LOW(WS-TX)
                   MOVE WS-SHIPPING-TIER-RATE(WS-TX) TO FXNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4820-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF FXNM-LAST-MAINT-DATE > 0 AND FXNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE FXNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD FXNT-FOREIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4830-COUNT-SEQ.
           IF FXNT-SEQ < 9999999
               ADD 1 TO FXNT-SEQ
           END-IF
           IF FXNT-SEQ NOT < 10
               MOVE 'Y' TO FXNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FOREIGN AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4840-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN FXNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN FXNM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-PENALTY-PCT / 200
               WHEN FXNM-SOURCE-CD = 'DG'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN FXNM-SOURCE-CD = 'FS'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = FXNM-FOREIGN-AMT *
                       FXNM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4850-SPLIT-BUYBACK.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE FXNM-BUYBACK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE USD SCHED TIER FOR THE BUYBACK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4860-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-USD-SCHED-RATE(1) TO FXNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF FXNM-BUYBACK-AMT NOT < WS-USD-SCHED-LOW(WS-TX)
                   MOVE WS-USD-SCHED-RATE(WS-TX) TO FXNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF FXNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO FXNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E420 CONTACT REQUIRED
      * E330 WORK AMT NEGATIVE
      * E373 WORK AMT OVER 50000
      * E428 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4870-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN FXNM-CONTACT = SPACES
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN FXNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE USD YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4880-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = FXNT-TRAN-CD
                   ADD FXNM-USD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE FXNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE FXNM-USD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * BUYBACK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4890-FLAG-BUYBACK.
           IF FXNM-BUYBACK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE FXNM-BUYBACK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE FXN FIELDS TO FNVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-FNVAL.
           INITIALIZE FNVAL-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNVAL-FOREIGN-AMT
           MOVE FXNM-SHIPPING-AMT
             TO FNVAL-SHIPPING-AMT
           MOVE FXNM-FOREIGN-YTD
             TO FNVAL-FOREIGN-YTD
           MOVE FXNM-SHIPPING-YTD
             TO FNVAL-SHIPPING-YTD
           MOVE FXNM-MARGIN-AMT
             TO FNVAL-MARGIN-AMT
           MOVE FXNM-USD-AMT
             TO FNVAL-USD-AMT
           MOVE FXNM-OPEN-DATE
             TO FNVAL-OPEN-DATE
           MOVE FXNM-EXPIRY-DATE
             TO FNVAL-EXPIRY-DATE
           MOVE FXNM-NOTICE-DATE
             TO FNVAL-NOTICE-DATE
           MOVE FXNM-LAST-ACT-DATE
             TO FNVAL-LAST-ACT-DATE
           MOVE FXNM-TYPE-CD
             TO FNVAL-TYPE-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNVAL-ORDER-TYPE-CD
           MOVE FXNM-SOURCE-CD
             TO FNVAL-SOURCE-CD
           MOVE FXNM-BRANCH-CD
             TO FNVAL-BRANCH-CD
           MOVE FXNM-EXEMPT-FLAG
             TO FNVAL-EXEMPT-FLAG
           MOVE FXNM-NOTICE-SENT-FLAG
             TO FNVAL-NOTICE-SENT-FLAG
           MOVE FXNM-REVIEW-FLAG
             TO FNVAL-REVIEW-FLAG
           MOVE FXNM-EXCEPT-CNT
             TO FNVAL-EXCEPT-CNT
           MOVE FXNM-PAYMENT-CNT
             TO FNVAL-PAYMENT-CNT
           MOVE FXNM-PENALTY-PCT
             TO FNVAL-PENALTY-PCT
           MOVE FXNM-RATE
             TO FNVAL-RATE
           MOVE FXNM-CONTACT
             TO FNVAL-CONTACT
           MOVE FXNM-REF-NO
             TO FNVAL-REF-NO
           CALL 'FNVAL' USING FNVAL-PARMS
           EVALUATE FNVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNVAL-FOREIGN-AMT
                     TO FXNM-FOREIGN-AMT
                   MOVE FNVAL-SHIPPING-AMT
                     TO FXNM-SHIPPING-AMT
                   MOVE FNVAL-MARGIN-AMT
                     TO FXNM-MARGIN-AMT
                   MOVE FNVAL-USD-AMT
                     TO FXNM-USD-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-FNVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE FXN FIELDS TO FNTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-FNTIER.
           INITIALIZE FNTIER-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNTIER-FOREIGN-AMT
           MOVE FXNM-USD-AMT
             TO FNTIER-USD-AMT
           MOVE FXNM-SHIPPING-AMT
             TO FNTIER-SHIPPING-AMT
           MOVE FXNM-BUYBACK-AMT
             TO FNTIER-BUYBACK-AMT
           MOVE FXNM-BUYBACK-YTD
             TO FNTIER-BUYBACK-YTD
           MOVE FXNM-FOREIGN-YTD
             TO FNTIER-FOREIGN-YTD
           MOVE FXNM-CLOSE-DATE
             TO FNTIER-CLOSE-DATE
           MOVE FXNM-LAST-MAINT-DATE
             TO FNTIER-LAST-MAINT-DATE
           MOVE FXNM-OPEN-DATE
             TO FNTIER-OPEN-DATE
           MOVE FXNM-EFFECTIVE-DATE
             TO FNTIER-EFFECTIVE-DATE
           MOVE FXNM-STATUS-CD
             TO FNTIER-STATUS-CD
           MOVE FXNM-SOURCE-CD
             TO FNTIER-SOURCE-CD
           MOVE FXNM-TYPE-CD
             TO FNTIER-TYPE-CD
           MOVE FXNM-BRANCH-CD
             TO FNTIER-BRANCH-CD
           MOVE FXNM-AUTO-RENEW-FLAG
             TO FNTIER-AUTO-RENEW-FLAG
           MOVE FXNM-NOTICE-SENT-FLAG
             TO FNTIER-NOTICE-SENT-FLAG
           MOVE FXNM-PREFERRED-FLAG
             TO FNTIER-PREFERRED-FLAG
           MOVE FXNM-PAYMENT-CNT
             TO FNTIER-PAYMENT-CNT
           MOVE FXNM-EXCEPT-CNT
             TO FNTIER-EXCEPT-CNT
           MOVE FXNM-MARGIN
             TO FNTIER-MARGIN
           MOVE FXNM-RATE
             TO FNTIER-RATE
           MOVE FXNM-DESC
             TO FNTIER-DESC
           MOVE FXNM-REF-NO
             TO FNTIER-REF-NO
           CALL 'FNTIER' USING FNTIER-PARMS
           EVALUATE FNTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE FNTIER-FOREIGN-AMT
                     TO FXNM-FOREIGN-AMT
                   MOVE FNTIER-USD-AMT
                     TO FXNM-USD-AMT
                   MOVE FNTIER-BUYBACK-AMT
                     TO FXNM-BUYBACK-AMT
                   MOVE FNTIER-BUYBACK-YTD
                     TO FXNM-BUYBACK-YTD
                   MOVE FNTIER-AUTO-RENEW-FLAG
                     TO FXNM-AUTO-RENEW-FLAG
                   MOVE FNTIER-NOTICE-SENT-FLAG
                     TO FXNM-NOTICE-SENT-FLAG
                   MOVE FNTIER-PREFERRED-FLAG
                     TO FXNM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-FNTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE FXNT-KEY TO RD-KEY
           MOVE FXNT-TRAN-CD TO RD-CODE
           MOVE FXNT-MARGIN-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE FXNT-KEY TO RD-KEY
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
           DISPLAY 'FNNSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'FNNSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
