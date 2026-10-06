      *================================================================*
      * PROGRAM  : CIMSWEP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHECK IMAGE ARCHIVE - SWEEP. READS THE CIM          *
      *            TRANSACTIONS, UPDATES THE CIM MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CHECK IMAGE CAPTURE INDEX,      *
      *            RETRIEVAL AND IMAGE STATEMENT FEES                  *
      * INPUT    : CIMMST (KSDS), CIMTRN, BANKCTL                      *
      * OUTPUT   : CIMOUT, CIMSWRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CIRISK, CIVAL, CIPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-10-04 RJT  ORIGINAL                                       *
      * 1995-11-24 JLM  STORAGE NOTICE TEXT PER LEGAL CHG6004          *
      * 1996-06-05 DPS  STORAGE LIMIT REVISED INC0679                  *
      * 1996-08-02 GAF  RETRIEVE-FEE FEE SCHEDULE UPDATE CHG6064       *
      * 1996-10-04 EWS  NEW IMAGE-FEE REPORT COLUMNS CHG9037           *
      * 1997-07-06 JLM  STORAGE ROUNDING - HALF UP PRB8382             *
      * 1999-04-08 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-10-25 TWH  RETRIEVE-FEE RERUN PROTECTION REQ9663          *
      * 2008-05-16 PQR  AUDIT FINDING - IMAGE-FEE CONTROL TOTALS REQ343*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIMSWEP.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "CIMSWRPT"
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
                                           VALUE 12.000000.
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
      * WS-STORAGE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000893500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000809000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000858000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000056500'.
       01  WS-STORAGE-BAND-TBL REDEFINES WS-STORAGE-BAND-VALUES.
           05  WS-STORAGE-BAND-ROW OCCURS 4 TIMES.
               10  WS-STORAGE-BAND-LOW     PIC 9(09)V99.
               10  WS-STORAGE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ITEM-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000044000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000404000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000264500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000496500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013550000000253000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000852000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018600000000548000'.
       01  WS-ITEM-TIER-TBL REDEFINES WS-ITEM-TIER-VALUES.
           05  WS-ITEM-TIER-ROW OCCURS 7 TIMES.
               10  WS-ITEM-TIER-LOW        PIC 9(09)V99.
               10  WS-ITEM-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ITEM-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000803000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000517500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000152000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000450000000053500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002950000000552000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003450000000586000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000183500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000797000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004650000000833500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007150000000464500'.
       01  WS-ITEM-GRID-TBL REDEFINES WS-ITEM-GRID-VALUES.
           05  WS-ITEM-GRID-ROW OCCURS 10 TIMES.
               10  WS-ITEM-GRID-LOW        PIC 9(09)V99.
               10  WS-ITEM-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-IMAGE-FE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-IMAGE-FE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000748500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000774500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000564500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000618000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000695500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000209000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018600000000582500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019100000000411000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019600000000217500'.
       01  WS-IMAGE-FE-BAND-TBL REDEFINES WS-IMAGE-FE-BAND-VALUES.
           05  WS-IMAGE-FE-BAND-ROW OCCURS 9 TIMES.
               10  WS-IMAGE-FE-BAND-LOW    PIC 9(09)V99.
               10  WS-IMAGE-FE-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CIMSWEP  CHECK IMAGE ARCHIVE'.
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
       COPY CIRISKP.
       COPY CIVALP.
       COPY CIPRCP.

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
               PERFORM 3000-ACCRUE-STORAGE
               PERFORM 3010-COUNT-SEQ
               PERFORM 3020-EDIT-ADDR
               PERFORM 3030-PRORATE-RETRIEVE-FEE
               PERFORM 3040-AGE-MATURITY
               PERFORM 3050-DESCRIBE-CHANNEL
               PERFORM 3060-CHECK-OPEN
               PERFORM 3070-PRORATE-IMAGE-FEE
               PERFORM 3080-SELECT-MARGIN
               PERFORM 3090-SET-CAPTURE-SITE
               PERFORM 3100-LIMIT-STORAGE
               PERFORM 3110-FLAG-STORAGE
               PERFORM 3120-SELECT-PENALTY-PCT
               PERFORM 3130-COUNT-PAYMENT
               PERFORM 3140-SET-STATUS
               PERFORM 3150-ACCRUE-STORAGE
               PERFORM 3160-LIMIT-ITEM
               PERFORM 3170-EDIT-MEMO
               PERFORM 3180-AGE-EFFECTIVE
               PERFORM 3190-DESCRIBE-STATUS
               PERFORM 3200-SELECT-MARGIN
               PERFORM 3210-AGE-NEXT-DUE
               PERFORM 3220-ACCRUE-MARGIN
               PERFORM 3230-ASSESS-REVERSAL
               PERFORM 3240-CHECK-OPEN
               PERFORM 3250-EDIT-ADDR
               PERFORM 3260-SPLIT-IMAGE-FEE
               PERFORM 3270-ACCUM-ITEM-TYPE
               PERFORM 3280-SPLIT-STORAGE
               PERFORM 3290-DESCRIBE-CHANNEL
               PERFORM 3300-SET-CHANNEL
               PERFORM 3310-EDIT-REJECT
               PERFORM 3320-PRORATE-RETRIEVE-FEE
               PERFORM 3330-PRORATE-IMAGE-FEE
               PERFORM 3340-LIMIT-IMAGE-FEE
               PERFORM 3350-ACCUM-CAPTURE-SITE
               PERFORM 3360-ACCRUE-RETRIEVE-FEE
               PERFORM 3370-CHECK-EFF
               PERFORM 3380-DESCRIBE-CHANNEL
               PERFORM 3390-SET-BRANCH
               PERFORM 3400-SET-STATUS
               PERFORM 3410-AGE-EFFECTIVE
               PERFORM 3420-COUNT-NOTICE
               PERFORM 3430-ACCUM-TRAN
               PERFORM 3440-ASSESS-HOLD
               PERFORM 3450-ACCRUE-MARGIN
               PERFORM 3460-ACCRUE-RETRIEVE-FEE
               PERFORM 3470-ACCRUE-ITEM
               PERFORM 3480-ACCRUE-STORAGE
               PERFORM 3490-COUNT-PAYMENT
               PERFORM 3500-FLAG-IMAGE-FEE
               PERFORM 3510-FLAG-ITEM
               PERFORM 3520-ACCRUE-STORAGE
               PERFORM 3530-CHECK-MATURITY
               PERFORM 3540-PRORATE-ITEM
               PERFORM 3550-CHECK-MATURITY
               PERFORM 3560-ACCRUE-ITEM
               PERFORM 3570-SPLIT-STORAGE
               PERFORM 3580-ACCRUE-RETRIEVE-FEE
               PERFORM 3590-AGE-POST
               PERFORM 3600-ASSESS-REVERSAL
               PERFORM 3610-FLAG-ITEM
               PERFORM 3620-SET-CHANNEL
               PERFORM 3630-SELECT-PENALTY-PCT
               PERFORM 3640-EDIT-ADDR
               PERFORM 3650-LIMIT-ITEM
               PERFORM 3660-CHECK-NOTICE
               PERFORM 3670-SELECT-MARGIN
               PERFORM 3680-ASSESS-REVIEW
               PERFORM 3690-AGE-OPEN
               PERFORM 3700-EDIT-DESC
               PERFORM 3710-COUNT-PAYMENT
               PERFORM 3720-COUNT-EXCEPT
               PERFORM 3730-PRORATE-RETRIEVE-FEE
               PERFORM 3740-LIMIT-ITEM
               PERFORM 3750-LIMIT-ITEM
               PERFORM 3760-PRORATE-IMAGE-FEE
               PERFORM 3770-SELECT-PENALTY-PCT
               PERFORM 3780-SELECT-PENALTY-PCT
               PERFORM 3790-EDIT-REJECT
               PERFORM 3800-LIMIT-IMAGE-FEE
               PERFORM 3810-DESCRIBE-TRAN
               PERFORM 3820-EDIT-REF-NO
               PERFORM 3830-SPLIT-RETRIEVE-FEE
               PERFORM 3840-LIMIT-IMAGE-FEE
               PERFORM 3850-SPLIT-IMAGE-FEE
               PERFORM 3860-ACCUM-CAPTURE-SITE
               PERFORM 3870-SET-BRANCH
               PERFORM 3880-EDIT-DESC
               PERFORM 3890-LIMIT-STORAGE
               PERFORM 3900-ASSESS-SUSPEND
               PERFORM 3910-ACCUM-CHANNEL
               PERFORM 3920-SELECT-FEE-PCT
               PERFORM 3930-DESCRIBE-BRANCH
               PERFORM 3940-ACCUM-TYPE
               PERFORM 3950-ACCRUE-RETRIEVE-FEE
               PERFORM 3960-ACCRUE-ITEM
               PERFORM 3970-PRORATE-IMAGE-FEE
               PERFORM 3980-AGE-OPEN
               PERFORM 3990-SPLIT-STORAGE
               PERFORM 4000-SELECT-PENALTY-PCT
               PERFORM 4010-DESCRIBE-ITEM-TYPE
               PERFORM 4020-ACCUM-CHANNEL
               PERFORM 4030-EDIT-CUST-ID
               PERFORM 4040-ACCUM-CHANNEL
               PERFORM 4050-ACCUM-BRANCH
               PERFORM 4060-SPLIT-RETRIEVE-FEE
               PERFORM 4070-SPLIT-STORAGE
               PERFORM 4080-SET-BRANCH
               PERFORM 4090-PRORATE-ITEM
               PERFORM 4100-CHECK-NEXT-DUE
               PERFORM 4110-SPLIT-IMAGE-FEE
               PERFORM 4120-ACCUM-BRANCH
               PERFORM 4130-AGE-EFFECTIVE
               PERFORM 4140-ACCRUE-RETRIEVE-FEE
               PERFORM 4150-PRORATE-ITEM
               PERFORM 4160-COUNT-PAYMENT
               PERFORM 4170-SET-CHANNEL
               PERFORM 4180-LIMIT-IMAGE-FEE
               PERFORM 4190-AGE-NEXT-REVIEW
               PERFORM 4200-FLAG-STORAGE
               PERFORM 4210-ASSESS-OVERRIDE
               PERFORM 4220-PRORATE-STORAGE
               PERFORM 4230-FLAG-ITEM
               PERFORM 4240-SPLIT-ITEM
               PERFORM 4250-PRORATE-STORAGE
               PERFORM 4260-SPLIT-ITEM
               PERFORM 4270-FLAG-IMAGE-FEE
               PERFORM 4280-AGE-NEXT-REVIEW
               PERFORM 4290-ACCUM-CHANNEL
               PERFORM 4300-AGE-POST
               PERFORM 4310-FLAG-ITEM
               PERFORM 4320-PRORATE-ITEM
               PERFORM 4330-COUNT-SEQ
               PERFORM 4340-LIMIT-STORAGE
               PERFORM 4350-EDIT-MEMO
               PERFORM 4360-LIMIT-ITEM
               PERFORM 4370-ACCUM-CHANNEL
               PERFORM 4380-EDIT-MEMO
               PERFORM 4390-COUNT-EXCEPT
               PERFORM 4400-DESCRIBE-STATUS
               PERFORM 4410-EDIT-CUST-ID
               PERFORM 4420-FLAG-ITEM
               PERFORM 4430-AGE-EFFECTIVE
               PERFORM 4440-SELECT-FEE-PCT
               PERFORM 4450-AGE-POST
               PERFORM 4460-SPLIT-RETRIEVE-FEE
               PERFORM 4470-PRORATE-ITEM
               PERFORM 4480-CHECK-MATURITY
               PERFORM 4490-CHECK-CLOSE
               PERFORM 4500-SPLIT-IMAGE-FEE
               PERFORM 4510-FLAG-ITEM
               PERFORM 4520-AGE-EFF
               PERFORM 4530-DESCRIBE-ITEM-TYPE
               PERFORM 4540-PRORATE-IMAGE-FEE
               PERFORM 4550-ASSESS-SUSPEND
               PERFORM 4560-SPLIT-ITEM
               PERFORM 4570-ACCUM-CHANNEL
               PERFORM 4580-AGE-POST
               PERFORM 4590-FLAG-RETRIEVE-FEE
               PERFORM 4600-AGE-POST
               PERFORM 4610-AGE-POST
               PERFORM 4620-FLAG-IMAGE-FEE
               PERFORM 4630-PRORATE-IMAGE-FEE
               PERFORM 4640-AGE-NEXT-REVIEW
               PERFORM 7000-CALL-CIRISK
               PERFORM 7010-CALL-CIVAL
               PERFORM 7020-CALL-CIPRC
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
      * DAILY INTEREST ON STORAGE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-STORAGE.
           IF CIMM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-YTD * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 6
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E317 ADDR REQUIRED
      * E918 STORAGE AMT NEGATIVE
      * E933 STORAGE AMT OVER 5000
      * E096 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN CIMT-STORAGE-AMT < 0
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN CIMT-STORAGE-AMT > 5000.00
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN CIMM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3030-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CIMM-RETRIEVE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-MATURITY.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-OPEN.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       3070-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ITEM TIER TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-TIER-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMM-ITEM-AMT NOT < WS-ITEM-TIER-LOW(WS-TX)
                   MOVE WS-ITEM-TIER-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CAPTURE SITE CD:
      * BB -> CA WHEN LAST STMT DATE IS PAST
      * CA -> PC WHEN LAST STMT DATE IS PAST
      * PC -> BB WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BB' TO CIMM-CAPTURE-SITE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3100-LIMIT-STORAGE.
           IF CIMM-STORAGE-YTD < 0.00
               MOVE 0.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-YTD > 2500.00
               MOVE 2500.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-STORAGE.
           IF CIMM-STORAGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM TIER TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-TIER-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMT-RETRIEVE-FEE-AMT NOT < WS-ITEM-TIER-LOW(WS-TX)
                   MOVE WS-ITEM-TIER-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN NOTICE DATE IS PAST
      * JY -> RU WHEN NOTICE DATE IS PAST
      * RU -> UR WHEN NOTICE DATE IS PAST
      * UR -> YW WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YW' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-STORAGE.
           IF CIMM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-AMT * CIMM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIMT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-ITEM.
           IF CIMM-ITEM-YTD < 0.00
               MOVE 0.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-YTD > 1000.00
               MOVE 1000.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E376 MEMO REQUIRED
      * E214 ITEM AMT NEGATIVE
      * E087 ITEM AMT OVER 250000
      * E448 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3170-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-MEMO = SPACES
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT < 0
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT > 250000.00
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN CIMM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-EFFECTIVE.
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
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIMT-STORAGE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-NEXT-DUE.
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
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CIMM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3230-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-OPEN.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E466 ADDR REQUIRED
      * E354 RETRIEVE FEE AMT NEGATIVE
      * E047 RETRIEVE FEE AMT OVER 99999
      * E022 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E466' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-AMT < 0
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-AMT > 99999.00
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN CIMM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMM-IMAGE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-ITEM-TYPE-CD
                   ADD CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMM-STORAGE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-STORAGE-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E505 REJECT CD REQUIRED
      * E168 STORAGE YTD NEGATIVE
      * E988 STORAGE YTD OVER 1000000
      * E942 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-REJECT-CD = SPACES
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-YTD < 0
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-YTD > 1000000.00
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN CIMM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       3320-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE YTD.
      *----------------------------------------------------------------*
       3330-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                   CIMT-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 0.00
               MOVE 0.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 5000.00
               MOVE 5000.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY CAPTURE SITE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CAPTURE-SITE-CD
                   ADD CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * CIMM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN MATURITY DATE IS PAST
      * FX -> MT WHEN MATURITY DATE IS PAST
      * MT -> UA WHEN MATURITY DATE IS PAST
      * UA -> VL WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FX'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN MATURITY DATE IS PAST
      * JY -> RU WHEN MATURITY DATE IS PAST
      * RU -> UR WHEN MATURITY DATE IS PAST
      * UR -> YW WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-STATUS.
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
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-EFFECTIVE.
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM YTD FEE ON THE RETRIEVE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3440-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIMM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-YTD ROUNDED = CIMM-RETRIEVE-FEE-AMT
                       * CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 5.00 TO CIMM-ITEM-YTD
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 10.00 TO CIMM-ITEM-YTD
               WHEN OTHER
                   COMPUTE CIMM-ITEM-YTD ROUNDED = CIMM-RETRIEVE-FEE-AMT
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-ITEM-YTD > 0 AND CIMM-ITEM-YTD < 2.00
               MOVE 2.00 TO CIMM-ITEM-YTD
           END-IF
           IF CIMM-ITEM-YTD > 35.00
               MOVE 35.00 TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMM-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-ITEM.
           IF CIMM-ITEM-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-YTD * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-STORAGE.
           IF CIMT-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-STORAGE-AMT * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-RETRIEVE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3500-FLAG-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMM-IMAGE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-STORAGE.
           IF CIMM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-YTD * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMT-ITEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIMM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIMM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-ITEM.
           IF CIMM-ITEM-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-YTD * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-RETRIEVE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE CIMT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3600-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMT-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED = CIMT-STORAGE-AMT
                       * CIMM-PENALTY-PCT / 200
               WHEN CIMT-TRAN-CD = 'CP'
                   MOVE 5.00 TO CIMT-IMAGE-FEE-AMT
               WHEN CIMT-TRAN-CD = 'RT'
                   MOVE 10.00 TO CIMT-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED = CIMT-STORAGE-AMT
                       * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMT-IMAGE-FEE-AMT > 0 AND CIMT-IMAGE-FEE-AMT < 1.00
               MOVE 1.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 75.00
               MOVE 75.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3610-FLAG-ITEM.
           IF CIMM-ITEM-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-ITEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3630-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CIMT-ITEM-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E222 ADDR REQUIRED
      * E082 STORAGE AMT NEGATIVE
      * E795 STORAGE AMT OVER 1000000
      * E015 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT < 0
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT > 1000000.00
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN CIMT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3650-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-NOTICE-DATE TO DTU-DATE-1
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
      * SELECT THE STORAGE BAND TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CIMM-STORAGE-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE STORAGE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3680-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMM-STORAGE-YTD *
                       CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 5.00 TO CIMM-STORAGE-AMT
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 10.00 TO CIMM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMM-STORAGE-YTD *
                       CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-STORAGE-AMT > 0 AND CIMM-STORAGE-AMT < 5.00
               MOVE 5.00 TO CIMM-STORAGE-AMT
           END-IF
           IF CIMM-STORAGE-AMT > 100.00
               MOVE 100.00 TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-OPEN.
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
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E042 DESC REQUIRED
      * E835 RETRIEVE FEE AMT NEGATIVE
      * E036 RETRIEVE FEE AMT OVER 10000
      * E241 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-DESC = SPACES
                   MOVE 'E042' TO WS-REJECT-CODE
               WHEN CIMT-RETRIEVE-FEE-AMT < 0
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN CIMT-RETRIEVE-FEE-AMT > 10000.00
                   MOVE 'E036' TO WS-REJECT-CODE
               WHEN CIMM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED =
                   CIMT-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMM-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3740-LIMIT-ITEM.
           IF CIMM-ITEM-AMT < 100.00
               MOVE 100.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-AMT > 500.00
               MOVE 500.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3760-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CIMT-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE BAND TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CIMM-ITEM-YTD NOT < WS-IMAGE-FE-BAND-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM TIER TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-TIER-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMT-STORAGE-AMT NOT < WS-ITEM-TIER-LOW(WS-TX)
                   MOVE WS-ITEM-TIER-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E990 REJECT CD REQUIRED
      * E052 IMAGE FEE YTD NEGATIVE
      * E197 IMAGE FEE YTD OVER 99999
      * E537 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-REJECT-CD = SPACES
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 99999.00
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN CIMM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3800-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT < 25.00
               MOVE 25.00 TO CIMM-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-AMT > 2500.00
               MOVE 2500.00 TO CIMM-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ST'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E523 REF NO REQUIRED
      * E025 ITEM YTD NEGATIVE
      * E818 ITEM YTD OVER 1000000
      * E009 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-REF-NO = SPACES
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-YTD < 0
                   MOVE 'E025' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-YTD > 1000000.00
                   MOVE 'E818' TO WS-REJECT-CODE
               WHEN CIMT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-RETRIEVE-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-RETRIEVE-FEE-YTD - (WS-WORK-AMT
               * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3840-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD < 10.00
               MOVE 10.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 2500.00
               MOVE 2500.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE CIMM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CAPTURE-SITE-CD
                   ADD CIMM-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN NOTICE DATE IS PAST
      * FX -> MT WHEN NOTICE DATE IS PAST
      * MT -> UA WHEN NOTICE DATE IS PAST
      * UA -> VL WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FX'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF CIMM-NOTICE-DATE > 0 AND CIMM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E430 DESC REQUIRED
      * E838 IMAGE FEE YTD NEGATIVE
      * E608 IMAGE FEE YTD OVER 50000
      * E004 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-DESC = SPACES
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E838' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 50000.00
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E004' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-STORAGE.
           IF CIMM-STORAGE-YTD < 50.00
               MOVE 50.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-YTD > 10000.00
               MOVE 10000.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE RETRIEVE FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3900-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   COMPUTE CIMT-STORAGE-AMT ROUNDED =
                       CIMM-RETRIEVE-FEE-AMT * CIMM-MARGIN / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 10.00 TO CIMT-STORAGE-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 10.00 TO CIMT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMT-STORAGE-AMT ROUNDED =
                       CIMM-RETRIEVE-FEE-AMT * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMT-STORAGE-AMT > 0 AND CIMT-STORAGE-AMT < 5.00
               MOVE 5.00 TO CIMT-STORAGE-AMT
           END-IF
           IF CIMT-STORAGE-AMT > 150.00
               MOVE 150.00 TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CHANNEL-CD
                   ADD CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIMM-ITEM-YTD NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-TYPE-CD
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-ITEM.
           IF CIMM-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-AMT * CIMM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                   CIMT-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-OPEN.
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
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CIMT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CIMM-ITEM-YTD NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMT-CHANNEL-CD
                   ADD CIMM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E948 CUST ID REQUIRED
      * E102 IMAGE FEE YTD NEGATIVE
      * E856 IMAGE FEE YTD OVER 10000
      * E053 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CUST-ID = SPACES
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 10000.00
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN CIMM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E053' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CHANNEL-CD
                   ADD CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-RETRIEVE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-RETRIEVE-FEE-YTD - (WS-WORK-AMT
               * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMM-STORAGE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-STORAGE-AMT - (WS-WORK-AMT * 3)
           COMPUTE CIMT-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN POST DATE IS PAST
      * FX -> MT WHEN POST DATE IS PAST
      * MT -> UA WHEN POST DATE IS PAST
      * UA -> VL WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FX'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4090-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMT-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-BRANCH-CD
                   ADD CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-EFFECTIVE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4160-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CIMM-LAST-STMT-DATE > 0 AND CIMM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4180-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD < 25.00
               MOVE 25.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 500.00
               MOVE 500.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CIMM-NEXT-REVIEW-DATE > 0 AND CIMM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4200-FLAG-STORAGE.
           IF CIMM-STORAGE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4210-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       CIMM-MARGIN / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       CIMM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE YTD.
      *----------------------------------------------------------------*
       4220-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-STORAGE-YTD ROUNDED =
                   CIMM-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-STORAGE-YTD
           END-IF
           ADD CIMM-STORAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4230-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4240-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMT-ITEM-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMT-ITEM-AMT - (WS-WORK-AMT * 3)
           COMPUTE CIMM-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE YTD.
      *----------------------------------------------------------------*
       4250-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-STORAGE-YTD ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-STORAGE-YTD
           END-IF
           ADD CIMM-STORAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4260-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-AMT - (WS-WORK-AMT * 4)
           COMPUTE CIMM-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4270-FLAG-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4280-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CIMM-NEXT-REVIEW-DATE > 0 AND CIMM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4310-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       4320-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-ITEM-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-ITEM-AMT
           END-IF
           ADD CIMT-ITEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4330-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 10
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4340-LIMIT-STORAGE.
           IF CIMM-STORAGE-AMT < 10.00
               MOVE 10.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-AMT > 500.00
               MOVE 500.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E091 MEMO REQUIRED
      * E275 RETRIEVE FEE YTD NEGATIVE
      * E813 RETRIEVE FEE YTD OVER 99999
      * E363 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4350-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-MEMO = SPACES
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-YTD < 0
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-YTD > 99999.00
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN CIMM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4360-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4370-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMT-CHANNEL-CD
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E392 MEMO REQUIRED
      * E714 ITEM AMT NEGATIVE
      * E465 ITEM AMT OVER 50000
      * E168 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4380-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-MEMO = SPACES
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT < 0
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT > 50000.00
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4390-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4400-DESCRIBE-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E623 CUST ID REQUIRED
      * E361 IMAGE FEE AMT NEGATIVE
      * E974 IMAGE FEE AMT OVER 25000
      * E455 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CUST-ID = SPACES
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN CIMT-IMAGE-FEE-AMT < 0
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN CIMT-IMAGE-FEE-AMT > 25000.00
                   MOVE 'E974' TO WS-REJECT-CODE
               WHEN CIMM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4420-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4430-AGE-EFFECTIVE.
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
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE BAND TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-BAND-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CIMM-ITEM-YTD NOT < WS-IMAGE-FE-BAND-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-BAND-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-RETRIEVE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       4470-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-YTD ROUNDED =
                   CIMT-ITEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CIMM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4490-CHECK-CLOSE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO CIMM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4500-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE CIMM-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4510-FLAG-ITEM.
           IF CIMM-ITEM-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-ITEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4530-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       4540-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4550-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMM-IMAGE-FEE-AMT *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMT-TRAN-CD = 'CP'
                   MOVE 3.00 TO CIMM-ITEM-AMT
               WHEN CIMT-TRAN-CD = 'RT'
                   MOVE 3.00 TO CIMM-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMM-IMAGE-FEE-AMT *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-ITEM-AMT > 0 AND CIMM-ITEM-AMT < 10.00
               MOVE 10.00 TO CIMM-ITEM-AMT
           END-IF
           IF CIMM-ITEM-AMT > 150.00
               MOVE 150.00 TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE CIMT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE YTD BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4570-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CHANNEL-CD
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-RETRIEVE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4580-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-IMAGE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4590-FLAG-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMT-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4600-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4610-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4620-FLAG-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE YTD.
      *----------------------------------------------------------------*
       4630-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                   CIMM-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4640-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CIMM-NEXT-REVIEW-DATE > 0 AND CIMM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CIM FIELDS TO CIRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-CIRISK.
           INITIALIZE CIRISK-PARMS
           MOVE CIMM-RETRIEVE-FEE-AMT
             TO CIRISK-RETRIEVE-FEE-AMT
           MOVE CIMM-IMAGE-FEE-AMT
             TO CIRISK-IMAGE-FEE-AMT
           MOVE CIMM-STORAGE-YTD
             TO CIRISK-STORAGE-YTD
           MOVE CIMM-RETRIEVE-FEE-YTD
             TO CIRISK-RETRIEVE-FEE-YTD
           MOVE CIMM-STORAGE-AMT
             TO CIRISK-STORAGE-AMT
           MOVE CIMM-ITEM-YTD
             TO CIRISK-ITEM-YTD
           MOVE CIMM-LAST-MAINT-DATE
             TO CIRISK-LAST-MAINT-DATE
           MOVE CIMM-CLOSE-DATE
             TO CIRISK-CLOSE-DATE
           MOVE CIMM-NEXT-DUE-DATE
             TO CIRISK-NEXT-DUE-DATE
           MOVE CIMM-OPEN-DATE
             TO CIRISK-OPEN-DATE
           MOVE CIMM-TYPE-CD
             TO CIRISK-TYPE-CD
           MOVE CIMM-ITEM-TYPE-CD
             TO CIRISK-ITEM-TYPE-CD
           MOVE CIMM-STATUS-CD
             TO CIRISK-STATUS-CD
           MOVE CIMM-CAPTURE-SITE-CD
             TO CIRISK-CAPTURE-SITE-CD
           MOVE CIMM-REVIEW-FLAG
             TO CIRISK-REVIEW-FLAG
           MOVE CIMM-HOLD-FLAG
             TO CIRISK-HOLD-FLAG
           MOVE CIMM-OVERRIDE-FLAG
             TO CIRISK-OVERRIDE-FLAG
           MOVE CIMM-PAYMENT-CNT
             TO CIRISK-PAYMENT-CNT
           MOVE CIMM-NOTICE-CNT
             TO CIRISK-NOTICE-CNT
           MOVE CIMM-MARGIN
             TO CIRISK-MARGIN
           MOVE CIMM-FEE-PCT
             TO CIRISK-FEE-PCT
           MOVE CIMM-NAME
             TO CIRISK-NAME
           MOVE CIMM-REF-NO
             TO CIRISK-REF-NO
           CALL 'CIRISK' USING CIRISK-PARMS
           EVALUATE CIRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CIRISK-RETRIEVE-FEE-AMT
                     TO CIMM-RETRIEVE-FEE-AMT
                   MOVE CIRISK-RETRIEVE-FEE-YTD
                     TO CIMM-RETRIEVE-FEE-YTD
                   MOVE CIRISK-ITEM-YTD
                     TO CIMM-ITEM-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-CIRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE CIVAL-RETRIEVE-FEE-AMT
                     TO CIMM-RETRIEVE-FEE-AMT
                   MOVE CIVAL-STORAGE-YTD
                     TO CIMM-STORAGE-YTD
                   MOVE CIVAL-ITEM-YTD
                     TO CIMM-ITEM-YTD
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

      *----------------------------------------------------------------*
      * PASS THE CIM FIELDS TO CIPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-CIPRC.
           INITIALIZE CIPRC-PARMS
           MOVE CIMM-IMAGE-FEE-AMT
             TO CIPRC-IMAGE-FEE-AMT
           MOVE CIMM-ITEM-AMT
             TO CIPRC-ITEM-AMT
           MOVE CIMM-RETRIEVE-FEE-YTD
             TO CIPRC-RETRIEVE-FEE-YTD
           MOVE CIMM-RETRIEVE-FEE-AMT
             TO CIPRC-RETRIEVE-FEE-AMT
           MOVE CIMM-ITEM-YTD
             TO CIPRC-ITEM-YTD
           MOVE CIMM-STORAGE-AMT
             TO CIPRC-STORAGE-AMT
           MOVE CIMM-NEXT-DUE-DATE
             TO CIPRC-NEXT-DUE-DATE
           MOVE CIMM-NOTICE-DATE
             TO CIPRC-NOTICE-DATE
           MOVE CIMM-EFFECTIVE-DATE
             TO CIPRC-EFFECTIVE-DATE
           MOVE CIMM-MATURITY-DATE
             TO CIPRC-MATURITY-DATE
           MOVE CIMM-STATUS-CD
             TO CIPRC-STATUS-CD
           MOVE CIMM-CAPTURE-SITE-CD
             TO CIPRC-CAPTURE-SITE-CD
           MOVE CIMM-CHANNEL-CD
             TO CIPRC-CHANNEL-CD
           MOVE CIMM-ITEM-TYPE-CD
             TO CIPRC-ITEM-TYPE-CD
           MOVE CIMM-EXEMPT-FLAG
             TO CIPRC-EXEMPT-FLAG
           MOVE CIMM-PREFERRED-FLAG
             TO CIPRC-PREFERRED-FLAG
           MOVE CIMM-SUSPEND-FLAG
             TO CIPRC-SUSPEND-FLAG
           MOVE CIMM-NOTICE-CNT
             TO CIPRC-NOTICE-CNT
           MOVE CIMM-RETRY-CNT
             TO CIPRC-RETRY-CNT
           MOVE CIMM-PENALTY-PCT
             TO CIPRC-PENALTY-PCT
           MOVE CIMM-MARGIN
             TO CIPRC-MARGIN
           MOVE CIMM-REF-NO
             TO CIPRC-REF-NO
           MOVE CIMM-ADDR
             TO CIPRC-ADDR
           CALL 'CIPRC' USING CIPRC-PARMS
           EVALUATE CIPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CIPRC-ITEM-AMT
                     TO CIMM-ITEM-AMT
                   MOVE CIPRC-ITEM-YTD
                     TO CIMM-ITEM-YTD
                   MOVE CIPRC-PREFERRED-FLAG
                     TO CIMM-PREFERRED-FLAG
                   MOVE CIPRC-SUSPEND-FLAG
                     TO CIMM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CIPRC' TO ABND-PARAGRAPH
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
           DISPLAY 'CIMSWEP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CIMSWEP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
