      *================================================================*
      * PROGRAM  : LBXYREN                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : LOCKBOX REMITTANCE - YEAR-END. READS THE LBX        *
      *            TRANSACTIONS, UPDATES THE LBX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. WHOLESALE LOCKBOX REMITTANCE    *
      *            CAPTURE AND CREDIT TO CLIENTS                       *
      * INPUT    : LBXMST (KSDS), LBXTRN, BANKCTL                      *
      * OUTPUT   : LBXOUT, LBXYRRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, LBCAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-21 SKN  ORIGINAL                                       *
      * 1996-09-09 LMB  FLOAT TABLE EXPANDED CHG6912                   *
      * 1998-06-15 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-04-10 PQR  REMIT STATUS CODES ADDED PRB9581               *
      * 2004-06-14 CRP  CHECK NOTICE TEXT PER LEGAL PRB8178            *
      * 2009-11-17 GAF  FLOAT CALCULATION CORRECTED REQ6730            *
      * 2010-02-04 CRP  EXCEPT STATUS CODES ADDED REQ8620              *
      * 2012-12-06 CRP  DEPOSIT LIMIT REVISED INC2674                  *
      * 2012-11-10 TWH  REMIT LIMIT REVISED REQ7164                    *
      * 2013-03-20 MKL  REMIT CALCULATION CORRECTED PRB9475            *
      * 2013-03-25 JLM  REMIT CUTOFF MOVED REQ8616                     *
      * 2014-10-02 PQR  EXCEPT FEE SCHEDULE UPDATE PRB3451             *
      * 2014-12-10 SKN  NEW CASH-ON-HAND REPORT COLUMNS INC7442        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBXYREN.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "LBXMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LBXM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "LBXTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "LBXOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "LBXYRRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY LBXMST.
       FD  TRAN-FILE.
       COPY LBXTRN.
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
                                           VALUE 20.000000.
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
      * WS-CHECK-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CHECK-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000836000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000169500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000382000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000017000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000757000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000775500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015700000000055000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018200000000686000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018450000000564500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028450000000345500'.
       01  WS-CHECK-BAND-TBL REDEFINES WS-CHECK-BAND-VALUES.
           05  WS-CHECK-BAND-ROW OCCURS 10 TIMES.
               10  WS-CHECK-BAND-LOW       PIC 9(09)V99.
               10  WS-CHECK-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FLOAT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FLOAT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000199000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000518000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000534000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000022000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000102000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000470000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022300000000313000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022800000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027800000000478000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037800000000818000'.
       01  WS-FLOAT-BAND-TBL REDEFINES WS-FLOAT-BAND-VALUES.
           05  WS-FLOAT-BAND-ROW OCCURS 10 TIMES.
               10  WS-FLOAT-BAND-LOW       PIC 9(09)V99.
               10  WS-FLOAT-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EXCEPT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCEPT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000606000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000225000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000439000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000411500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025100000000260500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025350000000244500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030350000000020000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040350000000844000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040850000000859000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00041850000000398000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00051850000000782000'.
       01  WS-EXCEPT-TIER-TBL REDEFINES WS-EXCEPT-TIER-VALUES.
           05  WS-EXCEPT-TIER-ROW OCCURS 11 TIMES.
               10  WS-EXCEPT-TIER-LOW      PIC 9(09)V99.
               10  WS-EXCEPT-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REMIT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REMIT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000501000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000045500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000373000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000783000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000750000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016750000000229000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026750000000773000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029250000000469000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031750000000163500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032250000000390000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034750000000365000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039750000000603000'.
       01  WS-REMIT-GRID-TBL REDEFINES WS-REMIT-GRID-VALUES.
           05  WS-REMIT-GRID-ROW OCCURS 12 TIMES.
               10  WS-REMIT-GRID-LOW       PIC 9(09)V99.
               10  WS-REMIT-GRID-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  LBXYREN  LOCKBOX REMITTANCE'.
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
       COPY LBCALP.

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
               MOVE 'LBXMST' TO ABND-FILE-NAME
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
           MOVE LBXT-KEY TO LBXM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-CHANNEL
               PERFORM 3010-ACCUM-CHANNEL
               PERFORM 3020-ACCRUE-FEE
               PERFORM 3030-AGE-LAST-PAY
               PERFORM 3040-SELECT-DISCOUNT
               PERFORM 3050-SELECT-DISCOUNT
               PERFORM 3060-ASSESS-PREFERRED
               PERFORM 3070-SET-CYCLE
               PERFORM 3080-PRORATE-CHECK
               PERFORM 3090-ASSESS-EXEMPT
               PERFORM 3100-ASSESS-PREFERRED
               PERFORM 3110-SET-REASON
               PERFORM 3120-AGE-NOTICE
               PERFORM 3130-COUNT-RETRY
               PERFORM 3140-DESCRIBE-LOCKBOX
               PERFORM 3150-SPLIT-EXCEPT
               PERFORM 3160-SELECT-PENALTY-PCT
               PERFORM 3170-ASSESS-REVERSAL
               PERFORM 3180-AGE-MATURITY
               PERFORM 3190-PRORATE-CASH-ON-HAND
               PERFORM 3200-LIMIT-ITEM
               PERFORM 3210-SET-CHANNEL
               PERFORM 3220-PRORATE-FEE
               PERFORM 3230-SET-REGION
               PERFORM 3240-CHECK-EFFECTIVE
               PERFORM 3250-ACCUM-CHANNEL
               PERFORM 3260-AGE-POST
               PERFORM 3270-ACCRUE-FLOAT
               PERFORM 3280-AGE-CLOSE
               PERFORM 3290-COUNT-SEQ
               PERFORM 3300-PRORATE-FLOAT
               PERFORM 3310-PRORATE-EXCEPT
               PERFORM 3320-SET-CHANNEL
               PERFORM 3330-CHECK-LAST-STMT
               PERFORM 3340-EDIT-MEMO
               PERFORM 3350-PRORATE-FEE
               PERFORM 3360-EDIT-CONTACT
               PERFORM 3370-DESCRIBE-CHANNEL
               PERFORM 3380-ASSESS-PREFERRED
               PERFORM 3390-SELECT-FEE-PCT
               PERFORM 3400-PRORATE-FLOAT
               PERFORM 3410-AGE-NOTICE
               PERFORM 3420-EDIT-OPERATOR
               PERFORM 3430-CHECK-OPEN
               PERFORM 3440-SELECT-PENALTY-PCT
               PERFORM 3450-SELECT-DISCOUNT
               PERFORM 3460-CHECK-POST
               PERFORM 3470-SPLIT-CHECK
               PERFORM 3480-FLAG-FLOAT
               PERFORM 3490-EDIT-REF-NO
               PERFORM 3500-FLAG-ITEM
               PERFORM 3510-SET-CHANNEL
               PERFORM 3520-SELECT-FEE-PCT
               PERFORM 3530-CHECK-MATURITY
               PERFORM 3540-LIMIT-ITEM
               PERFORM 3550-SPLIT-EXCEPT
               PERFORM 3560-FLAG-CHECK
               PERFORM 3570-EDIT-CUST-ID
               PERFORM 3580-ASSESS-HOLD
               PERFORM 3590-ACCRUE-FLOAT
               PERFORM 3600-ASSESS-AUTO-RENEW
               PERFORM 3610-ACCRUE-FLOAT
               PERFORM 3620-EDIT-REJECT
               PERFORM 3630-FLAG-FLOAT
               PERFORM 3640-AGE-EXPIRY
               PERFORM 3650-COUNT-ITEM
               PERFORM 3660-CHECK-LAST-PAY
               PERFORM 3670-SELECT-FEE-PCT
               PERFORM 3680-AGE-POST
               PERFORM 3690-FLAG-FEE
               PERFORM 3700-AGE-CLOSE
               PERFORM 3710-CHECK-OPEN
               PERFORM 3720-LIMIT-FEE
               PERFORM 3730-DESCRIBE-TRAN
               PERFORM 3740-EDIT-DESC
               PERFORM 3750-LIMIT-FEE
               PERFORM 3760-SPLIT-DEPOSIT
               PERFORM 3770-ASSESS-EXEMPT
               PERFORM 3780-ACCRUE-DISCOUNT
               PERFORM 3790-SET-REGION
               PERFORM 3800-ASSESS-PREFERRED
               PERFORM 3810-PRORATE-FEE
               PERFORM 3820-ACCRUE-FEE
               PERFORM 3830-COUNT-PAYMENT
               PERFORM 3840-ACCUM-BATCH-TYPE
               PERFORM 3850-EDIT-CONTACT
               PERFORM 3860-LIMIT-DEPOSIT
               PERFORM 3870-SELECT-PENALTY-PCT
               PERFORM 3880-ACCUM-CHANNEL
               PERFORM 3890-AGE-NEXT-DUE
               PERFORM 3900-COUNT-PAYMENT
               PERFORM 3910-FLAG-CHECK
               PERFORM 3920-SET-TRAN
               PERFORM 3930-ACCRUE-CASH-ON-HAND
               PERFORM 3940-PRORATE-CHECK
               PERFORM 3950-PRORATE-FLOAT
               PERFORM 3960-SPLIT-FEE
               PERFORM 3970-FLAG-REMIT
               PERFORM 3980-ACCUM-CHANNEL
               PERFORM 3990-ACCUM-REASON
               PERFORM 4000-PRORATE-CHECK
               PERFORM 4010-CHECK-EFFECTIVE
               PERFORM 4020-ASSESS-PREFERRED
               PERFORM 4030-ACCUM-CHANNEL
               PERFORM 4040-DESCRIBE-CHANNEL
               PERFORM 4050-PRORATE-FEE
               PERFORM 4060-DESCRIBE-REASON
               PERFORM 4070-FLAG-CHECK
               PERFORM 4080-PRORATE-DEPOSIT
               PERFORM 4090-ACCUM-REGION
               PERFORM 4100-DESCRIBE-REGION
               PERFORM 4110-SPLIT-DEPOSIT
               PERFORM 4120-FLAG-DEPOSIT
               PERFORM 4130-FLAG-FEE
               PERFORM 4140-SET-LOCKBOX
               PERFORM 4150-LIMIT-FLOAT
               PERFORM 4160-FLAG-FLOAT
               PERFORM 4170-LIMIT-REMIT
               PERFORM 4180-SELECT-DISCOUNT
               PERFORM 4190-SPLIT-FEE
               PERFORM 4200-FLAG-DEPOSIT
               PERFORM 4210-ACCUM-CHANNEL
               PERFORM 4220-AGE-CLOSE
               PERFORM 4230-ACCUM-CYCLE
               PERFORM 4240-PRORATE-FLOAT
               PERFORM 4250-ACCRUE-FEE
               PERFORM 4260-FLAG-ITEM
               PERFORM 4270-PRORATE-FEE
               PERFORM 4280-SELECT-PENALTY-PCT
               PERFORM 4290-COUNT-RETRY
               PERFORM 4300-CHECK-CLOSE
               PERFORM 4310-AGE-MATURITY
               PERFORM 4320-FLAG-FEE
               PERFORM 4330-ACCUM-BATCH-TYPE
               PERFORM 4340-ASSESS-HOLD
               PERFORM 4350-AGE-LAST-STMT
               PERFORM 4360-SPLIT-REMIT
               PERFORM 4370-ACCUM-REASON
               PERFORM 4380-SELECT-PENALTY-PCT
               PERFORM 4390-COUNT-SEQ
               PERFORM 4400-SPLIT-CHECK
               PERFORM 4410-EDIT-MEMO
               PERFORM 4420-PRORATE-CASH-ON-HAND
               PERFORM 4430-CHECK-CLOSE
               PERFORM 4440-COUNT-ITEM
               PERFORM 4450-DESCRIBE-REGION
               PERFORM 4460-SET-TRAN
               PERFORM 4470-COUNT-ITEM
               PERFORM 4480-LIMIT-FEE
               PERFORM 4490-ACCRUE-FLOAT
               PERFORM 4500-AGE-EFF
               PERFORM 4510-SPLIT-FLOAT
               PERFORM 4520-SPLIT-REMIT
               PERFORM 4530-SET-LOCKBOX
               PERFORM 4540-FLAG-CASH-ON-HAND
               PERFORM 4550-PRORATE-FLOAT
               PERFORM 4560-ACCRUE-EXCEPT
               PERFORM 4570-FLAG-DEPOSIT
               PERFORM 4580-EDIT-OPERATOR
               PERFORM 4590-ACCUM-LOCKBOX
               PERFORM 4600-ACCRUE-CHECK
               PERFORM 4610-EDIT-REF-NO
               PERFORM 4620-SELECT-FEE-PCT
               PERFORM 4630-SPLIT-EXCEPT
               PERFORM 4640-DESCRIBE-LOCKBOX
               PERFORM 4650-CHECK-MATURITY
               PERFORM 4660-ASSESS-EXEMPT
               PERFORM 4670-SPLIT-FEE
               PERFORM 4680-SET-REGION
               PERFORM 7010-CALL-LBCAL
               REWRITE LBX-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'LBXMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM LBX-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN CLOSE DATE IS PAST
      * H -> Z WHEN CLOSE DATE IS PAST
      * Z -> B WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-FEE.
           IF LBXM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FEE-AMT * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CHECK BAND TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXM-REMIT-YTD NOT < WS-CHECK-BAND-LOW(WS-TX)
                   MOVE WS-CHECK-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT GRID TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-GRID-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF LBXM-EXCEPT-AMT NOT < WS-REMIT-GRID-LOW(WS-TX)
                   MOVE WS-REMIT-GRID-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE EXCEPT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3060-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBXM-EXCEPT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-LOCKBOX-CD = 'DX'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN LBXM-LOCKBOX-CD = 'JM'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBXM-EXCEPT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EDV -> EPM WHEN EXPIRY DATE IS PAST
      * EPM -> FZY WHEN EXPIRY DATE IS PAST
      * FZY -> XUU WHEN EXPIRY DATE IS PAST
      * XUU -> YBH WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EPM' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EPM'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZY' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZY'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUU' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XUU'
                   IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YBH' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3080-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   LBXT-CHECK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FLOAT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3090-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = LBXM-FLOAT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN LBXT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = LBXM-FLOAT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3100-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-YTD ROUNDED = WS-WORK-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-LOCKBOX-CD = 'DX'
                   MOVE 10.00 TO LBXM-CASH-ON-HAND-YTD
               WHEN LBXM-LOCKBOX-CD = 'JM'
                   MOVE 12.00 TO LBXM-CASH-ON-HAND-YTD
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-YTD ROUNDED = WS-WORK-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-YTD > 0 AND LBXM-CASH-ON-HAND-YTD < 1.00
               MOVE 1.00 TO LBXM-CASH-ON-HAND-YTD
           END-IF
           IF LBXM-CASH-ON-HAND-YTD > 100.00
               MOVE 100.00 TO LBXM-CASH-ON-HAND-YTD
           END-IF
           ADD LBXM-CASH-ON-HAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN POST DATE IS PAST
      * UK -> VR WHEN POST DATE IS PAST
      * VR -> XG WHEN POST DATE IS PAST
      * XG -> GF WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 6
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBXM-EXCEPT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-EXCEPT-YTD - (WS-WORK-AMT * 3)
           COMPUTE LBXM-DEPOSIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-BAND-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXM-FEE-AMT NOT < WS-FLOAT-BAND-LOW(WS-TX)
                   MOVE WS-FLOAT-BAND-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE CHECK YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3170-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-CHECK-YTD * LBXM-PENALTY-PCT / 200
               WHEN LBXM-CYCLE-CD = 'EDV'
                   MOVE 15.00 TO LBXT-CASH-ON-HAND-AMT
               WHEN LBXM-CYCLE-CD = 'EPM'
                   MOVE 3.00 TO LBXT-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-CHECK-YTD * LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXT-CASH-ON-HAND-AMT > 0 AND LBXT-CASH-ON-HAND-AMT < 5.00
               MOVE 5.00 TO LBXT-CASH-ON-HAND-AMT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 75.00
               MOVE 75.00 TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ON HAND AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                   LBXM-REMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN EFFECTIVE DATE IS PAST
      * H -> Z WHEN EFFECTIVE DATE IS PAST
      * Z -> B WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3220-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   LBXT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN MATURITY DATE IS PAST
      * FMZ -> KSL WHEN MATURITY DATE IS PAST
      * KSL -> NYK WHEN MATURITY DATE IS PAST
      * NYK -> DVP WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-FLOAT.
           IF LBXT-FLOAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FLOAT-AMT * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 6
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FLOAT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCEPT YTD.
      *----------------------------------------------------------------*
       3310-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-EXCEPT-YTD ROUNDED =
                   LBXM-FLOAT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-EXCEPT-YTD
           END-IF
           ADD LBXM-EXCEPT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E496 MEMO REQUIRED
      * E602 FEE YTD NEGATIVE
      * E325 FEE YTD OVER 1000000
      * E112 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD < 0
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN LBXM-FEE-YTD > 1000000.00
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN LBXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3350-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   LBXT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E448 CONTACT REQUIRED
      * E256 DEPOSIT YTD NEGATIVE
      * E481 DEPOSIT YTD OVER 99999
      * E806 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD < 0
                   MOVE 'E256' TO WS-REJECT-CODE
               WHEN LBXM-DEPOSIT-YTD > 99999.00
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E806' TO WS-REJECT-CODE
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
       3370-DESCRIBE-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FLOAT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3380-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBXM-FLOAT-YTD *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = LBXM-FLOAT-YTD *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT TIER TIER FOR THE CASH ON HAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-TIER-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF LBXM-CASH-ON-HAND-AMT NOT < WS-EXCEPT-TIER-LOW(WS-TX)
                   MOVE WS-EXCEPT-TIER-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FLOAT YTD.
      *----------------------------------------------------------------*
       3400-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-FLOAT-YTD ROUNDED =
                   LBXM-FLOAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-FLOAT-YTD
           END-IF
           ADD LBXM-FLOAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E593 OPERATOR REQUIRED
      * E434 FLOAT YTD NEGATIVE
      * E755 FLOAT YTD OVER 1000000
      * E264 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-OPERATOR = SPACES
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD < 0
                   MOVE 'E434' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD > 1000000.00
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN LBXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHECK BAND TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-BAND-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXM-FEE-YTD NOT < WS-CHECK-BAND-LOW(WS-TX)
                   MOVE WS-CHECK-BAND-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHECK BAND TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXM-FEE-YTD NOT < WS-CHECK-BAND-LOW(WS-TX)
                   MOVE WS-CHECK-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHECK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXM-CHECK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-CHECK-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXT-CHECK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3480-FLAG-FLOAT.
           IF LBXT-FLOAT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXT-FLOAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 REF NO REQUIRED
      * E733 WORK AMT NEGATIVE
      * E780 WORK AMT OVER 50000
      * E142 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E733' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN LBXM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3500-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO LBXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT BAND TIER FOR THE CASH ON HAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-BAND-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXT-CASH-ON-HAND-AMT NOT < WS-FLOAT-BAND-LOW(WS-TX)
                   MOVE WS-FLOAT-BAND-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3540-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE LBXM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3560-FLAG-CHECK.
           IF LBXM-CHECK-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-CHECK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E910 CUST ID REQUIRED
      * E113 FLOAT AMT NEGATIVE
      * E181 FLOAT AMT OVER 25000
      * E110 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN LBXT-FLOAT-AMT < 0
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN LBXT-FLOAT-AMT > 25000.00
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN LBXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REMIT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3580-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = LBXM-REMIT-YTD *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-LOCKBOX-CD = 'DX'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN LBXM-LOCKBOX-CD = 'JM'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = LBXM-REMIT-YTD *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-FLOAT.
           IF LBXM-FLOAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-AMT * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE CHECK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3600-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-CHECK-AMT * LBXM-PENALTY-PCT / 200
               WHEN LBXM-REGION-CD = 'DVP'
                   MOVE 3.00 TO LBXT-CASH-ON-HAND-AMT
               WHEN LBXM-REGION-CD = 'FMZ'
                   MOVE 10.00 TO LBXT-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                       LBXM-CHECK-AMT * LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXT-CASH-ON-HAND-AMT > 0 AND LBXT-CASH-ON-HAND-AMT < 2.00
               MOVE 2.00 TO LBXT-CASH-ON-HAND-AMT
           END-IF
           IF LBXT-CASH-ON-HAND-AMT > 100.00
               MOVE 100.00 TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-FLOAT.
           IF LBXM-FLOAT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FLOAT-YTD * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXT-CHECK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E351 REJECT CD REQUIRED
      * E712 REMIT YTD NEGATIVE
      * E559 REMIT YTD OVER 99999
      * E550 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-REJECT-CD = SPACES
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-YTD < 0
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-YTD > 99999.00
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN LBXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3630-FLAG-FLOAT.
           IF LBXM-FLOAT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 6
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-TIER-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-EXCEPT-TIER-LOW(WS-TX)
                   MOVE WS-EXCEPT-TIER-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FLOAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3690-FLAG-FEE.
           IF LBXT-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-FEE.
           IF LBXM-FEE-YTD < 0.00
               MOVE 0.00 TO LBXM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FEE-YTD > 5000.00
               MOVE 5000.00 TO LBXM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E243 DESC REQUIRED
      * E994 FEE AMT NEGATIVE
      * E544 FEE AMT OVER 99999
      * E183 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-DESC = SPACES
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT < 0
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT > 99999.00
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN LBXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E183' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3750-LIMIT-FEE.
           IF LBXT-FEE-AMT < 10.00
               MOVE 10.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FEE-AMT > 500.00
               MOVE 500.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXM-DEPOSIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE FLOAT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3770-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-DEPOSIT-YTD ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 5.00 TO LBXM-DEPOSIT-YTD
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 15.00 TO LBXM-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE LBXM-DEPOSIT-YTD ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-DEPOSIT-YTD > 0 AND LBXM-DEPOSIT-YTD < 5.00
               MOVE 5.00 TO LBXM-DEPOSIT-YTD
           END-IF
           IF LBXM-DEPOSIT-YTD > 150.00
               MOVE 150.00 TO LBXM-DEPOSIT-YTD
           END-IF
           ADD LBXM-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN EFF DATE IS PAST
      * FMZ -> KSL WHEN EFF DATE IS PAST
      * KSL -> NYK WHEN EFF DATE IS PAST
      * NYK -> DVP WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND YTD FEE ON THE CASH ON HAND AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3800-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-STOP-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-YTD ROUNDED =
                       LBXM-CASH-ON-HAND-AMT * LBXM-DISCOUNT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 3.00 TO LBXM-CASH-ON-HAND-YTD
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 3.00 TO LBXM-CASH-ON-HAND-YTD
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-YTD ROUNDED =
                       LBXM-CASH-ON-HAND-AMT * LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-YTD > 0 AND LBXM-CASH-ON-HAND-YTD < 2.00
               MOVE 2.00 TO LBXM-CASH-ON-HAND-YTD
           END-IF
           IF LBXM-CASH-ON-HAND-YTD > 50.00
               MOVE 50.00 TO LBXM-CASH-ON-HAND-YTD
           END-IF
           ADD LBXM-CASH-ON-HAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3810-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-FEE-AMT ROUNDED =
                   LBXM-REMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXT-FEE-AMT
           END-IF
           ADD LBXT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY BATCH TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-BATCH-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-BATCH-TYPE-CD
                   ADD LBXM-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-BATCH-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E256 CONTACT REQUIRED
      * E912 FEE AMT NEGATIVE
      * E349 FEE AMT OVER 250000
      * E639 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CONTACT = SPACES
                   MOVE 'E256' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT < 0
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN LBXT-FEE-AMT > 250000.00
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN LBXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-YTD > 1000.00
               MOVE 1000.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHECK BAND TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CHECK-BAND-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXM-DEPOSIT-YTD NOT < WS-CHECK-BAND-LOW(WS-TX)
                   MOVE WS-CHECK-BAND-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXM-CHECK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHECK YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3910-FLAG-CHECK.
           IF LBXM-CHECK-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-CHECK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RC -> DP WHEN POST DATE IS PAST
      * DP -> EX WHEN POST DATE IS PAST
      * EX -> RJ WHEN POST DATE IS PAST
      * RJ -> AD WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ON HAND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CASH-ON-HAND-YTD * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CHECK-AMT ROUNDED =
                   LBXM-CASH-ON-HAND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-CHECK-AMT
           END-IF
           ADD LBXT-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FLOAT AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-FLOAT-AMT ROUNDED =
                   LBXT-FLOAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-FLOAT-AMT
           END-IF
           ADD LBXM-FLOAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE LBXM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3970-FLAG-REMIT.
           IF LBXM-REMIT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-REMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3980-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXM-EXCEPT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-EXCEPT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY REASON CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CHECK-AMT ROUNDED =
                   LBXM-REMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-CHECK-AMT
           END-IF
           ADD LBXT-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FLOAT YTD FEE ON THE REMIT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4020-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBXM-FLOAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-HOLD-FLAG = 'Y'
                   COMPUTE LBXM-FLOAT-YTD ROUNDED = LBXM-REMIT-AMT *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-REGION-CD = 'DVP'
                   MOVE 12.00 TO LBXM-FLOAT-YTD
               WHEN LBXM-REGION-CD = 'FMZ'
                   MOVE 5.00 TO LBXM-FLOAT-YTD
               WHEN OTHER
                   COMPUTE LBXM-FLOAT-YTD ROUNDED = LBXM-REMIT-AMT *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-FLOAT-YTD > 0 AND LBXM-FLOAT-YTD < 1.00
               MOVE 1.00 TO LBXM-FLOAT-YTD
           END-IF
           IF LBXM-FLOAT-YTD > 100.00
               MOVE 100.00 TO LBXM-FLOAT-YTD
           END-IF
           ADD LBXM-FLOAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD LBXT-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-CHANNEL.
           EVALUATE LBXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4050-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-FEE-YTD ROUNDED =
                   LBXM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-FEE-YTD
           END-IF
           ADD LBXM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHECK YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4070-FLAG-CHECK.
           IF LBXM-CHECK-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-CHECK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       4080-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-YTD ROUNDED =
                   LBXM-REMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-YTD
           END-IF
           ADD LBXM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FLOAT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REGION-CD
                   ADD LBXM-FLOAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-FLOAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBXM-DEPOSIT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-DEPOSIT-YTD - (WS-WORK-AMT * 3)
           COMPUTE LBXM-CASH-ON-HAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4120-FLAG-DEPOSIT.
           IF LBXM-DEPOSIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4130-FLAG-FEE.
           IF LBXM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN CLOSE DATE IS PAST
      * JM -> WW WHEN CLOSE DATE IS PAST
      * WW -> XE WHEN CLOSE DATE IS PAST
      * XE -> DX WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FLOAT YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4150-LIMIT-FLOAT.
           IF LBXM-FLOAT-YTD < 50.00
               MOVE 50.00 TO LBXM-FLOAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FLOAT-YTD > 10000.00
               MOVE 10000.00 TO LBXM-FLOAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FLOAT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4160-FLAG-FLOAT.
           IF LBXM-FLOAT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-FLOAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4170-LIMIT-REMIT.
           IF LBXM-REMIT-AMT < 0.00
               MOVE 0.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-REMIT-AMT > 5000.00
               MOVE 5000.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4180-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-TIER-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF LBXM-FEE-AMT NOT < WS-EXCEPT-TIER-LOW(WS-TX)
                   MOVE WS-EXCEPT-TIER-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4200-FLAG-DEPOSIT.
           IF LBXM-DEPOSIT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4230-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CYCLE-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FLOAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4240-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FLOAT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FLOAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4260-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4270-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE REMIT GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF LBXM-FEE-AMT NOT < WS-REMIT-GRID-LOW(WS-TX)
                   MOVE WS-REMIT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-RETRY.
           IF LBXM-RETRY-CNT < 9999999
               ADD 1 TO LBXM-RETRY-CNT
           END-IF
           IF LBXM-RETRY-CNT NOT < 10
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4310-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4320-FLAG-FEE.
           IF LBXM-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHECK YTD BY BATCH TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4330-ACCUM-BATCH-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-BATCH-TYPE-CD
                   ADD LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-BATCH-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CHECK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE CHECK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4340-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN LBXM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                       LBXT-CHECK-AMT * LBXM-FEE-PCT / 200
               WHEN LBXM-REGION-CD = 'DVP'
                   MOVE 20.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN LBXM-REGION-CD = 'FMZ'
                   MOVE 3.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED =
                       LBXT-CHECK-AMT * LBXM-FEE-PCT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-AMT > 0 AND LBXM-CASH-ON-HAND-AMT <
               10.00
               MOVE 10.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 100.00
               MOVE 100.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4350-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CHECK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4360-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4370-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT GRID TIER FOR THE CASH ON HAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4380-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-GRID-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF LBXT-CASH-ON-HAND-AMT NOT < WS-REMIT-GRID-LOW(WS-TX)
                   MOVE WS-REMIT-GRID-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4390-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 12
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHECK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXM-CHECK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-CHECK-AMT - (WS-WORK-AMT * 3)
           COMPUTE LBXM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E464 MEMO REQUIRED
      * E100 CASH ON HAND AMT NEGATIVE
      * E844 CASH ON HAND AMT OVER 1000000
      * E347 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-AMT < 0
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN LBXM-CASH-ON-HAND-AMT > 1000000.00
                   MOVE 'E844' TO WS-REJECT-CODE
               WHEN LBXM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E347' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHECK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ON HAND AMT.
      *----------------------------------------------------------------*
       4420-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-CASH-ON-HAND-AMT ROUNDED =
                   LBXM-CHECK-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXT-CASH-ON-HAND-AMT
           END-IF
           ADD LBXT-CASH-ON-HAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4430-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4440-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 5
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4450-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RC -> DP WHEN NOTICE DATE IS PAST
      * DP -> EX WHEN NOTICE DATE IS PAST
      * EX -> RJ WHEN NOTICE DATE IS PAST
      * RJ -> AD WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4460-SET-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF LBXM-NOTICE-DATE > 0 AND LBXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO LBXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4470-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 3
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4480-LIMIT-FEE.
           IF LBXT-FEE-AMT < 100.00
               MOVE 100.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-FEE-AMT > 2500.00
               MOVE 2500.00 TO LBXT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4490-ACCRUE-FLOAT.
           IF LBXT-FLOAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FLOAT-AMT * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXT-CHECK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-CHECK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FLOAT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4510-SPLIT-FLOAT.
           COMPUTE WS-WORK-AMT = LBXT-FLOAT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXT-FLOAT-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4520-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE LBXT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOCKBOX CD:
      * DX -> JM WHEN MATURITY DATE IS PAST
      * JM -> WW WHEN MATURITY DATE IS PAST
      * WW -> XE WHEN MATURITY DATE IS PAST
      * XE -> DX WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4530-SET-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WW' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WW'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XE' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XE'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO LBXM-LOCKBOX-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4540-FLAG-CASH-ON-HAND.
           IF LBXT-CASH-ON-HAND-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXT-CASH-ON-HAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FLOAT YTD.
      *----------------------------------------------------------------*
       4550-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-FLOAT-YTD ROUNDED =
                   LBXM-EXCEPT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-FLOAT-YTD
           END-IF
           ADD LBXM-FLOAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4560-ACCRUE-EXCEPT.
           IF LBXM-EXCEPT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-EXCEPT-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-CASH-ON-HAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4570-FLAG-DEPOSIT.
           IF LBXM-DEPOSIT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE LBXM-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E673 OPERATOR REQUIRED
      * E239 FEE AMT NEGATIVE
      * E753 FEE AMT OVER 50000
      * E086 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4580-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-OPERATOR = SPACES
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN LBXM-FEE-AMT < 0
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN LBXM-FEE-AMT > 50000.00
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND YTD BY LOCKBOX CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4590-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-LOCKBOX-CD
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4600-ACCRUE-CHECK.
           IF LBXM-CHECK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CHECK-YTD * LBXM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-FLOAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E753 REF NO REQUIRED
      * E198 REMIT YTD NEGATIVE
      * E588 REMIT YTD OVER 250000
      * E633 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4610-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-YTD < 0
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-YTD > 250000.00
                   MOVE 'E588' TO WS-REJECT-CODE
               WHEN LBXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FLOAT BAND TIER FOR THE FLOAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4620-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FLOAT-BAND-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF LBXT-FLOAT-AMT NOT < WS-FLOAT-BAND-LOW(WS-TX)
                   MOVE WS-FLOAT-BAND-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4630-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = LBXM-EXCEPT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-EXCEPT-AMT - (WS-WORK-AMT * 12)
           COMPUTE LBXM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOCKBOX CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4640-DESCRIBE-LOCKBOX.
           EVALUATE LBXM-LOCKBOX-CD
               WHEN 'DX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4650-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4660-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXT-FEE-AMT ROUNDED = LBXM-DEPOSIT-YTD *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-REGION-CD = 'DVP'
                   MOVE 12.00 TO LBXT-FEE-AMT
               WHEN LBXM-REGION-CD = 'FMZ'
                   MOVE 5.00 TO LBXT-FEE-AMT
               WHEN OTHER
                   COMPUTE LBXT-FEE-AMT ROUNDED = LBXM-DEPOSIT-YTD *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXT-FEE-AMT > 0 AND LBXT-FEE-AMT < 2.00
               MOVE 2.00 TO LBXT-FEE-AMT
           END-IF
           IF LBXT-FEE-AMT > 250.00
               MOVE 250.00 TO LBXT-FEE-AMT
           END-IF
           ADD LBXT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4670-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN POST DATE IS PAST
      * FMZ -> KSL WHEN POST DATE IS PAST
      * KSL -> NYK WHEN POST DATE IS PAST
      * NYK -> DVP WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4680-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-LBCAL.
           INITIALIZE LBCAL-PARMS
           MOVE LBXM-DEPOSIT-AMT
             TO LBCAL-DEPOSIT-AMT
           MOVE LBXM-DEPOSIT-YTD
             TO LBCAL-DEPOSIT-YTD
           MOVE LBXM-CHECK-AMT
             TO LBCAL-CHECK-AMT
           MOVE LBXM-CASH-ON-HAND-YTD
             TO LBCAL-CASH-ON-HAND-YTD
           MOVE LBXM-FLOAT-YTD
             TO LBCAL-FLOAT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBCAL-REMIT-YTD
           MOVE LBXM-NOTICE-DATE
             TO LBCAL-NOTICE-DATE
           MOVE LBXM-NEXT-DUE-DATE
             TO LBCAL-NEXT-DUE-DATE
           MOVE LBXM-OPEN-DATE
             TO LBCAL-OPEN-DATE
           MOVE LBXM-EFFECTIVE-DATE
             TO LBCAL-EFFECTIVE-DATE
           MOVE LBXM-REGION-CD
             TO LBCAL-REGION-CD
           MOVE LBXM-BATCH-TYPE-CD
             TO LBCAL-BATCH-TYPE-CD
           MOVE LBXM-REASON-CD
             TO LBCAL-REASON-CD
           MOVE LBXM-LOCKBOX-CD
             TO LBCAL-LOCKBOX-CD
           MOVE LBXM-HOLD-FLAG
             TO LBCAL-HOLD-FLAG
           MOVE LBXM-PREFERRED-FLAG
             TO LBCAL-PREFERRED-FLAG
           MOVE LBXM-NOTICE-SENT-FLAG
             TO LBCAL-NOTICE-SENT-FLAG
           MOVE LBXM-EXCEPT-CNT
             TO LBCAL-EXCEPT-CNT
           MOVE LBXM-RETRY-CNT
             TO LBCAL-RETRY-CNT
           MOVE LBXM-PENALTY-PCT
             TO LBCAL-PENALTY-PCT
           MOVE LBXM-DISCOUNT
             TO LBCAL-DISCOUNT
           MOVE LBXM-DESC
             TO LBCAL-DESC
           MOVE LBXM-CONTACT
             TO LBCAL-CONTACT
           CALL 'LBCAL' USING LBCAL-PARMS
           EVALUATE LBCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBCAL-CHECK-AMT
                     TO LBXM-CHECK-AMT
                   MOVE LBCAL-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBCAL-NOTICE-SENT-FLAG
                     TO LBXM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-LBCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE LBXT-KEY TO RD-KEY
           MOVE LBXT-TRAN-CD TO RD-CODE
           MOVE LBXT-FLOAT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE LBXT-KEY TO RD-KEY
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
           DISPLAY 'LBXYREN READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'LBXYREN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
