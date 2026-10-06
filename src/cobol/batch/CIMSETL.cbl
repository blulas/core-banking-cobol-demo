      *================================================================*
      * PROGRAM  : CIMSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHECK IMAGE ARCHIVE - SETTLEMENT. READS THE CIM     *
      *            TRANSACTIONS, UPDATES THE CIM MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CHECK IMAGE CAPTURE INDEX,      *
      *            RETRIEVAL AND IMAGE STATEMENT FEES                  *
      * INPUT    : CIMMST (KSDS), CIMTRN, BANKCTL                      *
      * OUTPUT   : CIMOUT, CIMSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CIVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-11-21 BHS  ORIGINAL                                       *
      * 1993-06-15 RJT  RETRIEVE-FEE EXCEPTIONS TO OPS QUEUE REQ9263   *
      * 1995-03-09 LMB  ITEM CALCULATION CORRECTED PRB7451             *
      * 1996-01-10 SKN  ITEM LIMIT REVISED CHG6038                     *
      * 1999-04-19 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-10-19 TWH  RETRIEVE-FEE TABLE EXPANDED PRB0605            *
      * 2002-01-24 TWH  NEW ITEM REPORT COLUMNS PRB7775                *
      * 2004-12-27 EWS  NEW RETRIEVE-FEE REPORT COLUMNS PRB3707        *
      * 2007-04-20 DPS  PERFORMANCE - RETRIEVE-FEE READ AHEAD PRB9175  *
      * 2009-11-27 PQR  STORAGE CUTOFF MOVED INC4600                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIMSETL.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "CIMMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CIMM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "CIMTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "CIMOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CIMSERPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY CIMMST.
       FD  TRAN-FILE.
       COPY CIMTRN.
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
                                           VALUE 21.000000.
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
      * WS-ITEM-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000216500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000317500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000411500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000784500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020350000000792000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000219500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000395000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026850000000751000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027350000000845000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027850000000617000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028350000000454000'.
       01  WS-ITEM-SCHED-TBL REDEFINES WS-ITEM-SCHED-VALUES.
           05  WS-ITEM-SCHED-ROW OCCURS 11 TIMES.
               10  WS-ITEM-SCHED-LOW       PIC 9(09)V99.
               10  WS-ITEM-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ITEM-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000384500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000086500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000655500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000655500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005200000000872000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005700000000164000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008200000000393000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000051000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000408000'.
       01  WS-ITEM-GRID-TBL REDEFINES WS-ITEM-GRID-VALUES.
           05  WS-ITEM-GRID-ROW OCCURS 9 TIMES.
               10  WS-ITEM-GRID-LOW        PIC 9(09)V99.
               10  WS-ITEM-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000432500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000710000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000383000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000459000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007050000000865000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000454000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000411000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017650000000014000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018150000000356000'.
       01  WS-STORAGE-GRID-TBL REDEFINES WS-STORAGE-GRID-VALUES.
           05  WS-STORAGE-GRID-ROW OCCURS 9 TIMES.
               10  WS-STORAGE-GRID-LOW     PIC 9(09)V99.
               10  WS-STORAGE-GRID-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CIMSETL  CHECK IMAGE ARCHIVE'.
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
       COPY CIVALP.

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
               MOVE 'CIMMST' TO ABND-FILE-NAME
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
           MOVE CIMT-KEY TO CIMM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-FLAG-STORAGE
               PERFORM 3010-ACCRUE-STORAGE
               PERFORM 3020-AGE-OPEN
               PERFORM 3030-SET-CAPTURE-SITE
               PERFORM 3040-ACCUM-STATUS
               PERFORM 3050-SET-STATUS
               PERFORM 3060-ASSESS-EXEMPT
               PERFORM 3070-EDIT-ADDR
               PERFORM 3080-PRORATE-IMAGE-FEE
               PERFORM 3090-CHECK-OPEN
               PERFORM 3100-CHECK-CLOSE
               PERFORM 3110-SET-CHANNEL
               PERFORM 3120-AGE-MATURITY
               PERFORM 3130-DESCRIBE-ITEM-TYPE
               PERFORM 3140-FLAG-RETRIEVE-FEE
               PERFORM 3150-CHECK-CLOSE
               PERFORM 3160-AGE-NOTICE
               PERFORM 3170-PRORATE-IMAGE-FEE
               PERFORM 3180-AGE-LAST-STMT
               PERFORM 3190-LIMIT-ITEM
               PERFORM 3200-COUNT-PAYMENT
               PERFORM 3210-SET-CHANNEL
               PERFORM 3220-DESCRIBE-CAPTURE-SITE
               PERFORM 3230-ACCRUE-STORAGE
               PERFORM 3240-SELECT-MARGIN
               PERFORM 3250-ASSESS-REVERSAL
               PERFORM 3260-SPLIT-STORAGE
               PERFORM 3270-SELECT-FEE-PCT
               PERFORM 3280-SELECT-MARGIN
               PERFORM 3290-AGE-LAST-STMT
               PERFORM 3300-SELECT-PENALTY-PCT
               PERFORM 3310-COUNT-PAYMENT
               PERFORM 3320-PRORATE-STORAGE
               PERFORM 3330-CHECK-POST
               PERFORM 3340-SPLIT-IMAGE-FEE
               PERFORM 3350-ASSESS-SUSPEND
               PERFORM 3360-PRORATE-IMAGE-FEE
               PERFORM 3370-SET-BRANCH
               PERFORM 3380-SPLIT-STORAGE
               PERFORM 3390-COUNT-EXCEPT
               PERFORM 3400-PRORATE-ITEM
               PERFORM 3410-ACCRUE-IMAGE-FEE
               PERFORM 3420-SPLIT-IMAGE-FEE
               PERFORM 3430-ACCUM-BRANCH
               PERFORM 3440-SET-CAPTURE-SITE
               PERFORM 3450-FLAG-IMAGE-FEE
               PERFORM 3460-CHECK-POST
               PERFORM 3470-ACCUM-STATUS
               PERFORM 3480-AGE-EFFECTIVE
               PERFORM 3490-LIMIT-IMAGE-FEE
               PERFORM 3500-DESCRIBE-CAPTURE-SITE
               PERFORM 3510-LIMIT-ITEM
               PERFORM 3520-ASSESS-REVIEW
               PERFORM 3530-CHECK-EFFECTIVE
               PERFORM 3540-COUNT-EXCEPT
               PERFORM 3550-SELECT-PENALTY-PCT
               PERFORM 3560-FLAG-RETRIEVE-FEE
               PERFORM 3570-LIMIT-RETRIEVE-FEE
               PERFORM 3580-COUNT-SEQ
               PERFORM 3590-COUNT-SEQ
               PERFORM 3600-FLAG-STORAGE
               PERFORM 3610-ASSESS-SUSPEND
               PERFORM 3620-ACCRUE-RETRIEVE-FEE
               PERFORM 3630-ASSESS-REVERSAL
               PERFORM 3640-COUNT-PAYMENT
               PERFORM 3650-DESCRIBE-CAPTURE-SITE
               PERFORM 3660-LIMIT-RETRIEVE-FEE
               PERFORM 3670-ACCRUE-STORAGE
               PERFORM 3680-SET-STATUS
               PERFORM 3690-SPLIT-RETRIEVE-FEE
               PERFORM 3700-PRORATE-RETRIEVE-FEE
               PERFORM 3710-FLAG-IMAGE-FEE
               PERFORM 3720-DESCRIBE-TYPE
               PERFORM 3730-LIMIT-STORAGE
               PERFORM 3740-ACCRUE-STORAGE
               PERFORM 3750-DESCRIBE-BRANCH
               PERFORM 3760-LIMIT-ITEM
               PERFORM 3770-FLAG-RETRIEVE-FEE
               PERFORM 3780-DESCRIBE-CHANNEL
               PERFORM 3790-CHECK-NEXT-DUE
               PERFORM 3800-COUNT-PAYMENT
               PERFORM 3810-SET-TRAN
               PERFORM 3820-ACCUM-CAPTURE-SITE
               PERFORM 3830-CHECK-EFF
               PERFORM 3840-SELECT-MARGIN
               PERFORM 3850-EDIT-CONTACT
               PERFORM 3860-AGE-NEXT-DUE
               PERFORM 3870-LIMIT-STORAGE
               PERFORM 3880-PRORATE-RETRIEVE-FEE
               PERFORM 3890-SET-BRANCH
               PERFORM 3900-EDIT-OPERATOR
               PERFORM 3910-COUNT-PAYMENT
               PERFORM 3920-SPLIT-STORAGE
               PERFORM 3930-LIMIT-STORAGE
               PERFORM 3940-ASSESS-REVERSAL
               PERFORM 3950-FLAG-STORAGE
               PERFORM 3960-ASSESS-EXEMPT
               PERFORM 3970-EDIT-CONTACT
               PERFORM 3980-FLAG-STORAGE
               PERFORM 3990-ASSESS-REVERSAL
               PERFORM 4000-PRORATE-STORAGE
               PERFORM 4010-ASSESS-OVERRIDE
               PERFORM 4020-ACCUM-CHANNEL
               PERFORM 4030-SELECT-PENALTY-PCT
               PERFORM 4040-PRORATE-IMAGE-FEE
               PERFORM 4050-PRORATE-IMAGE-FEE
               PERFORM 4060-DESCRIBE-CHANNEL
               PERFORM 4070-SELECT-FEE-PCT
               PERFORM 4080-EDIT-NAME
               PERFORM 4090-PRORATE-RETRIEVE-FEE
               PERFORM 4100-LIMIT-IMAGE-FEE
               PERFORM 4110-ACCRUE-MARGIN
               PERFORM 4120-DESCRIBE-BRANCH
               PERFORM 4130-CHECK-LAST-STMT
               PERFORM 4140-AGE-LAST-STMT
               PERFORM 4150-CHECK-OPEN
               PERFORM 4160-SET-ITEM-TYPE
               PERFORM 4170-ASSESS-OVERRIDE
               PERFORM 4180-FLAG-RETRIEVE-FEE
               PERFORM 4190-ASSESS-HOLD
               PERFORM 4200-EDIT-REF-NO
               PERFORM 4210-SELECT-FEE-PCT
               PERFORM 4220-ACCRUE-RETRIEVE-FEE
               PERFORM 4230-EDIT-OPERATOR
               PERFORM 4240-SPLIT-STORAGE
               PERFORM 4250-ACCRUE-ITEM
               PERFORM 4260-SET-TRAN
               PERFORM 4270-DESCRIBE-CHANNEL
               PERFORM 4280-SPLIT-RETRIEVE-FEE
               PERFORM 4290-SET-ITEM-TYPE
               PERFORM 4300-ACCUM-CAPTURE-SITE
               PERFORM 4310-ASSESS-OVERRIDE
               PERFORM 4320-PRORATE-STORAGE
               PERFORM 7010-CALL-CIVAL
               REWRITE CIM-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'CIMMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM CIM-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3000-FLAG-STORAGE.
           IF CIMM-STORAGE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-STORAGE.
           IF CIMM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-YTD * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CAPTURE SITE CD:
      * BB -> CA WHEN EFF DATE IS PAST
      * CA -> PC WHEN EFF DATE IS PAST
      * PC -> BB WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE YTD BY STATUS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-STATUS-CD
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN MATURITY DATE IS PAST
      * JY -> RU WHEN MATURITY DATE IS PAST
      * RU -> UR WHEN MATURITY DATE IS PAST
      * UR -> YW WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YW' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIMT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIMT-STORAGE-AMT ROUNDED = CIMM-IMAGE-FEE-AMT
                       * CIMM-MARGIN / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 12.00 TO CIMT-STORAGE-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 3.00 TO CIMT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMT-STORAGE-AMT ROUNDED = CIMM-IMAGE-FEE-AMT
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMT-STORAGE-AMT > 0 AND CIMT-STORAGE-AMT < 5.00
               MOVE 5.00 TO CIMT-STORAGE-AMT
           END-IF
           IF CIMT-STORAGE-AMT > 100.00
               MOVE 100.00 TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E883 ADDR REQUIRED
      * E756 WORK AMT NEGATIVE
      * E275 WORK AMT OVER 250000
      * E579 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN CIMM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO IMAGE FEE YTD.
      *----------------------------------------------------------------*
       3080-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CIMM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3140-FLAG-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3190-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FW -> KL WHEN EFF DATE IS PAST
      * KL -> NB WHEN EFF DATE IS PAST
      * NB -> TP WHEN EFF DATE IS PAST
      * TP -> ZZ WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KL' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KL'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NB' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NB'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TP'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZZ' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3220-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-STORAGE.
           IF CIMM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-YTD * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM SCHED TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-SCHED-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CIMM-ITEM-YTD NOT < WS-ITEM-SCHED-LOW(WS-TX)
                   MOVE WS-ITEM-SCHED-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE YTD FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3250-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                       CIMT-IMAGE-FEE-AMT * CIMM-FEE-PCT / 200
               WHEN CIMM-BRANCH-CD = 'DJ'
                   MOVE 12.00 TO CIMM-IMAGE-FEE-YTD
               WHEN CIMM-BRANCH-CD = 'FX'
                   MOVE 7.00 TO CIMM-IMAGE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                       CIMT-IMAGE-FEE-AMT * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-YTD > 0 AND CIMM-IMAGE-FEE-YTD < 5.00
               MOVE 5.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 75.00
               MOVE 75.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMM-STORAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CIMM-STORAGE-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CIMT-ITEM-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3320-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   CIMT-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF CIMT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CIMM-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE ITEM YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3350-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CIMM-ITEM-YTD *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CIMM-ITEM-YTD *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN OPEN DATE IS PAST
      * FX -> MT WHEN OPEN DATE IS PAST
      * MT -> UA WHEN OPEN DATE IS PAST
      * UA -> VL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FX'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMT-STORAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIMT-STORAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMT-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO CIMM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CIMT-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CAPTURE SITE CD:
      * BB -> CA WHEN MATURITY DATE IS PAST
      * CA -> PC WHEN MATURITY DATE IS PAST
      * PC -> BB WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3450-FLAG-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMM-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF CIMT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-STATUS-CD
                   ADD CIMT-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-EFFECTIVE-DATE > 0 AND CIMM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-EFFECTIVE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3490-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD < 50.00
               MOVE 50.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 1000.00
               MOVE 1000.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3510-LIMIT-ITEM.
           IF CIMM-ITEM-YTD < 10.00
               MOVE 10.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-YTD > 5000.00
               MOVE 5000.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE IMAGE FEE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3520-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMM-IMAGE-FEE-YTD *
                       CIMM-FEE-PCT / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 10.00 TO CIMM-ITEM-AMT
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 3.00 TO CIMM-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMM-IMAGE-FEE-YTD *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-ITEM-AMT > 0 AND CIMM-ITEM-AMT < 2.00
               MOVE 2.00 TO CIMM-ITEM-AMT
           END-IF
           IF CIMM-ITEM-AMT > 75.00
               MOVE 75.00 TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE IMAGE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CIMT-IMAGE-FEE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3570-LIMIT-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT < 50.00
               MOVE 50.00 TO CIMM-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-RETRIEVE-FEE-AMT > 500.00
               MOVE 500.00 TO CIMM-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 12
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 3
               MOVE 'Y' TO CIMM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3600-FLAG-STORAGE.
           IF CIMM-STORAGE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3610-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = WS-WORK-AMT *
                       CIMM-FEE-PCT / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 5.00 TO CIMM-IMAGE-FEE-YTD
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 15.00 TO CIMM-IMAGE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = WS-WORK-AMT *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-YTD > 0 AND CIMM-IMAGE-FEE-YTD < 1.00
               MOVE 1.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 150.00
               MOVE 150.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE STORAGE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3630-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMM-STORAGE-YTD *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMM-CAPTURE-SITE-CD = 'BB'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN CIMM-CAPTURE-SITE-CD = 'CA'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMM-STORAGE-YTD *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3660-LIMIT-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT < 50.00
               MOVE 50.00 TO CIMT-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-RETRIEVE-FEE-AMT > 1000.00
               MOVE 1000.00 TO CIMT-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-STORAGE.
           IF CIMM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-AMT * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMM-RETRIEVE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN OPEN DATE IS PAST
      * JY -> RU WHEN OPEN DATE IS PAST
      * RU -> UR WHEN OPEN DATE IS PAST
      * UR -> YW WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YW' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMT-RETRIEVE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CIMT-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 2)
           COMPUTE CIMM-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3710-FLAG-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-STORAGE.
           IF CIMT-STORAGE-AMT < 50.00
               MOVE 50.00 TO CIMT-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-STORAGE-AMT > 5000.00
               MOVE 5000.00 TO CIMT-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-STORAGE.
           IF CIMM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-YTD * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMT-ITEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-ITEM.
           IF CIMT-ITEM-AMT < 100.00
               MOVE 100.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-ITEM-AMT > 5000.00
               MOVE 5000.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3770-FLAG-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CP -> RT WHEN NOTICE DATE IS PAST
      * RT -> ST WHEN NOTICE DATE IS PAST
      * ST -> PG WHEN NOTICE DATE IS PAST
      * PG -> CP WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE YTD BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CAPTURE-SITE-CD
                   ADD CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF CIMT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-SCHED-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-ITEM-SCHED-LOW(WS-TX)
                   MOVE WS-ITEM-SCHED-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E526 CONTACT REQUIRED
      * E606 WORK AMT 2 NEGATIVE
      * E739 WORK AMT 2 OVER 25000
      * E875 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CONTACT = SPACES
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN CIMT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-STORAGE.
           IF CIMM-STORAGE-AMT < 0.00
               MOVE 0.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-AMT > 50000.00
               MOVE 50000.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3880-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMT-ITEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN LAST STMT DATE IS PAST
      * FX -> MT WHEN LAST STMT DATE IS PAST
      * MT -> UA WHEN LAST STMT DATE IS PAST
      * UA -> VL WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FX'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E709 OPERATOR REQUIRED
      * E802 IMAGE FEE YTD NEGATIVE
      * E791 IMAGE FEE YTD OVER 25000
      * E612 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-OPERATOR = SPACES
                   MOVE 'E709' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 25000.00
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN CIMM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMM-STORAGE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-STORAGE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CIMT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-STORAGE.
           IF CIMM-STORAGE-AMT < 50.00
               MOVE 50.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-AMT > 1000.00
               MOVE 1000.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE RETRIEVE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3940-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMT-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE CIMT-ITEM-AMT ROUNDED = CIMM-RETRIEVE-FEE-AMT
                       * CIMM-FEE-PCT / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 5.00 TO CIMT-ITEM-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 7.00 TO CIMT-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMT-ITEM-AMT ROUNDED = CIMM-RETRIEVE-FEE-AMT
                       * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMT-ITEM-AMT > 0 AND CIMT-ITEM-AMT < 10.00
               MOVE 10.00 TO CIMT-ITEM-AMT
           END-IF
           IF CIMT-ITEM-AMT > 100.00
               MOVE 100.00 TO CIMT-ITEM-AMT
           END-IF
           ADD CIMT-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3950-FLAG-STORAGE.
           IF CIMM-STORAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE YTD FEE ON THE STORAGE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3960-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = CIMM-STORAGE-YTD
                       * CIMM-FEE-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO CIMM-IMAGE-FEE-YTD
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO CIMM-IMAGE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = CIMM-STORAGE-YTD
                       * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-YTD > 0 AND CIMM-IMAGE-FEE-YTD < 5.00
               MOVE 5.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 75.00
               MOVE 75.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E828 CONTACT REQUIRED
      * E130 WORK AMT 2 NEGATIVE
      * E083 WORK AMT 2 OVER 99999
      * E358 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CONTACT = SPACES
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3980-FLAG-STORAGE.
           IF CIMM-STORAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3990-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMT-STORAGE-AMT *
                       CIMM-FEE-PCT / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 3.00 TO CIMM-ITEM-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 20.00 TO CIMM-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMT-STORAGE-AMT *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-ITEM-AMT > 0 AND CIMM-ITEM-AMT < 5.00
               MOVE 5.00 TO CIMM-ITEM-AMT
           END-IF
           IF CIMM-ITEM-AMT > 150.00
               MOVE 150.00 TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE YTD FEE ON THE STORAGE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4010-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = CIMM-STORAGE-AMT
                       * CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 12.00 TO CIMM-IMAGE-FEE-YTD
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 10.00 TO CIMM-IMAGE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = CIMM-STORAGE-AMT
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-YTD > 0 AND CIMM-IMAGE-FEE-YTD < 2.00
               MOVE 2.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 250.00
               MOVE 250.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMT-CHANNEL-CD
                   ADD CIMT-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-SCHED-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-ITEM-SCHED-LOW(WS-TX)
                   MOVE WS-ITEM-SCHED-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       4040-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-AMT
           END-IF
           ADD CIMM-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM SCHED TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-SCHED-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CIMM-STORAGE-AMT NOT < WS-ITEM-SCHED-LOW(WS-TX)
                   MOVE WS-ITEM-SCHED-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E995 NAME REQUIRED
      * E306 IMAGE FEE AMT NEGATIVE
      * E684 IMAGE FEE AMT OVER 250000
      * E274 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-NAME = SPACES
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-AMT < 0
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-AMT > 250000.00
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN CIMM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       4090-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMM-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4100-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 100.00
               MOVE 100.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 500.00
               MOVE 500.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF CIMM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CIMM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN MATURITY DATE IS PAST
      * FB -> HN WHEN MATURITY DATE IS PAST
      * HN -> KX WHEN MATURITY DATE IS PAST
      * KX -> CW WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE YTD FEE ON THE IMAGE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4170-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-YTD ROUNDED = CIMM-IMAGE-FEE-YTD
                       * CIMM-PENALTY-PCT / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 3.00 TO CIMM-STORAGE-YTD
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 3.00 TO CIMM-STORAGE-YTD
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-YTD ROUNDED = CIMM-IMAGE-FEE-YTD
                       * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-STORAGE-YTD > 0 AND CIMM-STORAGE-YTD < 1.00
               MOVE 1.00 TO CIMM-STORAGE-YTD
           END-IF
           IF CIMM-STORAGE-YTD > 50.00
               MOVE 50.00 TO CIMM-STORAGE-YTD
           END-IF
           ADD CIMM-STORAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4180-FLAG-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMT-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE ITEM AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4190-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIMM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CIMT-ITEM-AMT *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMM-CAPTURE-SITE-CD = 'BB'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN CIMM-CAPTURE-SITE-CD = 'CA'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CIMT-ITEM-AMT *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E771 REF NO REQUIRED
      * E957 IMAGE FEE YTD NEGATIVE
      * E877 IMAGE FEE YTD OVER 10000
      * E126 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-REF-NO = SPACES
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 10000.00
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN CIMM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CIMM-ITEM-YTD NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E232 OPERATOR REQUIRED
      * E781 WORK AMT NEGATIVE
      * E901 WORK AMT OVER 50000
      * E044 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-OPERATOR = SPACES
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN CIMM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4240-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMT-STORAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIMT-STORAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMT-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-ITEM.
           IF CIMM-ITEM-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-YTD * CIMM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIMT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CP -> RT WHEN NOTICE DATE IS PAST
      * RT -> ST WHEN NOTICE DATE IS PAST
      * ST -> PG WHEN NOTICE DATE IS PAST
      * PG -> CP WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4260-SET-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4270-DESCRIBE-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMT-RETRIEVE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CIMT-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN LAST STMT DATE IS PAST
      * FB -> HN WHEN LAST STMT DATE IS PAST
      * HN -> KX WHEN LAST STMT DATE IS PAST
      * KX -> CW WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM YTD BY CAPTURE SITE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4300-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CAPTURE-SITE-CD
                   ADD CIMM-ITEM-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-ITEM-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE IMAGE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4310-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMT-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                       CIMM-IMAGE-FEE-YTD * CIMM-MARGIN / 200
               WHEN CIMT-TRAN-CD = 'CP'
                   MOVE 3.00 TO CIMT-IMAGE-FEE-AMT
               WHEN CIMT-TRAN-CD = 'RT'
                   MOVE 10.00 TO CIMT-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                       CIMM-IMAGE-FEE-YTD * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMT-IMAGE-FEE-AMT > 0 AND CIMT-IMAGE-FEE-AMT < 1.00
               MOVE 1.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 150.00
               MOVE 150.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       4320-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   CIMM-STORAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE CIM FIELDS TO CIVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-CIVAL.
           INITIALIZE CIVAL-PARMS
           MOVE CIMM-IMAGE-FEE-AMT
             TO CIVAL-IMAGE-FEE-AMT
           MOVE CIMM-RETRIEVE-FEE-AMT
             TO CIVAL-RETRIEVE-FEE-AMT
           MOVE CIMM-STORAGE-AMT
             TO CIVAL-STORAGE-AMT
           MOVE CIMM-STORAGE-YTD
             TO CIVAL-STORAGE-YTD
           MOVE CIMM-ITEM-AMT
             TO CIVAL-ITEM-AMT
           MOVE CIMM-ITEM-YTD
             TO CIVAL-ITEM-YTD
           MOVE CIMM-OPEN-DATE
             TO CIVAL-OPEN-DATE
           MOVE CIMM-EFFECTIVE-DATE
             TO CIVAL-EFFECTIVE-DATE
           MOVE CIMM-LAST-STMT-DATE
             TO CIVAL-LAST-STMT-DATE
           MOVE CIMM-LAST-MAINT-DATE
             TO CIVAL-LAST-MAINT-DATE
           MOVE CIMM-BRANCH-CD
             TO CIVAL-BRANCH-CD
           MOVE CIMM-CHANNEL-CD
             TO CIVAL-CHANNEL-CD
           MOVE CIMM-ITEM-TYPE-CD
             TO CIVAL-ITEM-TYPE-CD
           MOVE CIMM-CAPTURE-SITE-CD
             TO CIVAL-CAPTURE-SITE-CD
           MOVE CIMM-EXEMPT-FLAG
             TO CIVAL-EXEMPT-FLAG
           MOVE CIMM-HOLD-FLAG
             TO CIVAL-HOLD-FLAG
           MOVE CIMM-OVERRIDE-FLAG
             TO CIVAL-OVERRIDE-FLAG
           MOVE CIMM-PAYMENT-CNT
             TO CIVAL-PAYMENT-CNT
           MOVE CIMM-NOTICE-CNT
             TO CIVAL-NOTICE-CNT
           MOVE CIMM-MARGIN
             TO CIVAL-MARGIN
           MOVE CIMM-PENALTY-PCT
             TO CIVAL-PENALTY-PCT
           MOVE CIMM-NAME
             TO CIVAL-NAME
           MOVE CIMM-ADDR
             TO CIVAL-ADDR
           CALL 'CIVAL' USING CIVAL-PARMS
           EVALUATE CIVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CIVAL-IMAGE-FEE-AMT
                     TO CIMM-IMAGE-FEE-AMT
                   MOVE CIVAL-STORAGE-AMT
                     TO CIMM-STORAGE-AMT
                   MOVE CIVAL-EXEMPT-FLAG
                     TO CIMM-EXEMPT-FLAG
                   MOVE CIVAL-HOLD-FLAG
                     TO CIMM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CIVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE CIMT-KEY TO RD-KEY
           MOVE CIMT-TRAN-CD TO RD-CODE
           MOVE CIMT-STORAGE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE CIMT-KEY TO RD-KEY
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
           DISPLAY 'CIMSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CIMSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
