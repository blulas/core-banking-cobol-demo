      *================================================================*
      * PROGRAM  : CIMPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHECK IMAGE ARCHIVE - DAILY POSTING. READS THE CIM  *
      *            TRANSACTIONS, UPDATES THE CIM MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CHECK IMAGE CAPTURE INDEX,      *
      *            RETRIEVAL AND IMAGE STATEMENT FEES                  *
      * INPUT    : CIMMST (KSDS), CIMTRN, BANKCTL                      *
      * OUTPUT   : CIMOUT, CIMPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CIRISK                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-06-04 BHS  ORIGINAL                                       *
      * 1994-06-08 TWH  ITEM CALCULATION CORRECTED REQ0160             *
      * 1999-06-08 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-08-22 BHS  IMAGE-FEE EXCEPTIONS TO OPS QUEUE REQ3685      *
      * 2000-02-21 BHS  STORAGE TABLE EXPANDED PRB8848                 *
      * 2001-05-27 BHS  IMAGE-FEE STATUS CODES ADDED REQ7935           *
      * 2003-06-05 PQR  PERFORMANCE - RETRIEVE-FEE READ AHEAD PRB3792  *
      * 2003-09-13 DPS  STORAGE CUTOFF MOVED INC1285                   *
      * 2006-08-13 CRP  PERFORMANCE - STORAGE READ AHEAD REQ5042       *
      * 2008-03-10 EWS  RETRIEVE-FEE LIMIT REVISED CHG9269             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIMPOST.
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
           SELECT REPORT-FILE ASSIGN TO "CIMPORPT"
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
                                           VALUE 20.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
      * WS-ITEM-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000254500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000313000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000892000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000868000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000480000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000667000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000402000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017250000000354000'.
       01  WS-ITEM-BAND-TBL REDEFINES WS-ITEM-BAND-VALUES.
           05  WS-ITEM-BAND-ROW OCCURS 8 TIMES.
               10  WS-ITEM-BAND-LOW        PIC 9(09)V99.
               10  WS-ITEM-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000173500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000848000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000262500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000066000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000020000'.
       01  WS-STORAGE-BAND-TBL REDEFINES WS-STORAGE-BAND-VALUES.
           05  WS-STORAGE-BAND-ROW OCCURS 5 TIMES.
               10  WS-STORAGE-BAND-LOW     PIC 9(09)V99.
               10  WS-STORAGE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-IMAGE-FE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-IMAGE-FE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000686500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000451000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000397000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000287000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000009000'.
       01  WS-IMAGE-FE-SCHED-TBL REDEFINES WS-IMAGE-FE-SCHED-VALUES.
           05  WS-IMAGE-FE-SCHED-ROW OCCURS 5 TIMES.
               10  WS-IMAGE-FE-SCHED-LOW   PIC 9(09)V99.
               10  WS-IMAGE-FE-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CIMPOST  CHECK IMAGE ARCHIVE'.
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
               PERFORM 3000-DESCRIBE-STATUS
               PERFORM 3010-DESCRIBE-TYPE
               PERFORM 3020-COUNT-RETRY
               PERFORM 3030-LIMIT-IMAGE-FEE
               PERFORM 3040-FLAG-ITEM
               PERFORM 3050-LIMIT-RETRIEVE-FEE
               PERFORM 3060-SET-STATUS
               PERFORM 3070-EDIT-CUST-ID
               PERFORM 3080-ACCUM-TYPE
               PERFORM 3090-AGE-MATURITY
               PERFORM 3100-COUNT-EXCEPT
               PERFORM 3110-SPLIT-ITEM
               PERFORM 3120-COUNT-RETRY
               PERFORM 3130-LIMIT-IMAGE-FEE
               PERFORM 3140-COUNT-RETRY
               PERFORM 3150-ACCUM-ITEM-TYPE
               PERFORM 3160-SELECT-PENALTY-PCT
               PERFORM 3170-ACCRUE-ITEM
               PERFORM 3180-AGE-OPEN
               PERFORM 3190-SELECT-PENALTY-PCT
               PERFORM 3200-ACCRUE-ITEM
               PERFORM 3210-AGE-LAST-MAINT
               PERFORM 3220-SPLIT-RETRIEVE-FEE
               PERFORM 3230-SELECT-MARGIN
               PERFORM 3240-SELECT-PENALTY-PCT
               PERFORM 3250-ACCUM-TRAN
               PERFORM 3260-ACCRUE-RETRIEVE-FEE
               PERFORM 3270-EDIT-DESC
               PERFORM 3280-ASSESS-SUSPEND
               PERFORM 3290-ACCRUE-IMAGE-FEE
               PERFORM 3300-SPLIT-ITEM
               PERFORM 3310-SET-CHANNEL
               PERFORM 3320-ASSESS-HOLD
               PERFORM 3330-ASSESS-PREFERRED
               PERFORM 3340-AGE-OPEN
               PERFORM 3350-EDIT-OPERATOR
               PERFORM 3360-PRORATE-STORAGE
               PERFORM 3370-ACCRUE-ITEM
               PERFORM 3380-SET-ITEM-TYPE
               PERFORM 3390-SET-ITEM-TYPE
               PERFORM 3400-PRORATE-ITEM
               PERFORM 3410-COUNT-NOTICE
               PERFORM 3420-PRORATE-STORAGE
               PERFORM 3430-SPLIT-STORAGE
               PERFORM 3440-CHECK-EFFECTIVE
               PERFORM 3450-FLAG-ITEM
               PERFORM 3460-SET-TRAN
               PERFORM 3470-ACCRUE-PENALTY-PCT
               PERFORM 3480-EDIT-NAME
               PERFORM 3490-AGE-CLOSE
               PERFORM 3500-DESCRIBE-CHANNEL
               PERFORM 3510-PRORATE-RETRIEVE-FEE
               PERFORM 3520-LIMIT-RETRIEVE-FEE
               PERFORM 3530-LIMIT-ITEM
               PERFORM 3540-FLAG-STORAGE
               PERFORM 3550-AGE-LAST-STMT
               PERFORM 3560-SELECT-FEE-PCT
               PERFORM 3570-ASSESS-HOLD
               PERFORM 3580-EDIT-DESC
               PERFORM 3590-SPLIT-IMAGE-FEE
               PERFORM 3600-ASSESS-REVERSAL
               PERFORM 3610-ACCUM-BRANCH
               PERFORM 3620-ACCUM-STATUS
               PERFORM 3630-COUNT-NOTICE
               PERFORM 3640-ASSESS-HOLD
               PERFORM 3650-SELECT-MARGIN
               PERFORM 3660-PRORATE-ITEM
               PERFORM 3670-ASSESS-SUSPEND
               PERFORM 3680-AGE-EFFECTIVE
               PERFORM 3690-DESCRIBE-TYPE
               PERFORM 3700-PRORATE-RETRIEVE-FEE
               PERFORM 3710-CHECK-NEXT-DUE
               PERFORM 3720-ACCRUE-PENALTY-PCT
               PERFORM 3730-AGE-LAST-STMT
               PERFORM 3740-ASSESS-OVERRIDE
               PERFORM 3750-CHECK-NEXT-DUE
               PERFORM 3760-CHECK-OPEN
               PERFORM 3770-AGE-MATURITY
               PERFORM 3780-AGE-MATURITY
               PERFORM 3790-DESCRIBE-ITEM-TYPE
               PERFORM 3800-LIMIT-IMAGE-FEE
               PERFORM 3810-LIMIT-IMAGE-FEE
               PERFORM 3820-DESCRIBE-STATUS
               PERFORM 3830-ASSESS-REVIEW
               PERFORM 3840-SELECT-PENALTY-PCT
               PERFORM 3850-CHECK-MATURITY
               PERFORM 3860-PRORATE-IMAGE-FEE
               PERFORM 3870-PRORATE-RETRIEVE-FEE
               PERFORM 3880-PRORATE-ITEM
               PERFORM 3890-DESCRIBE-STATUS
               PERFORM 3900-EDIT-DESC
               PERFORM 3910-SPLIT-STORAGE
               PERFORM 3920-DESCRIBE-TYPE
               PERFORM 3930-COUNT-NOTICE
               PERFORM 3940-FLAG-RETRIEVE-FEE
               PERFORM 3950-ASSESS-SUSPEND
               PERFORM 3960-LIMIT-STORAGE
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-ASSESS-PREFERRED
               PERFORM 3990-CHECK-OPEN
               PERFORM 4000-EDIT-ADDR
               PERFORM 4010-ASSESS-EXEMPT
               PERFORM 4020-ACCUM-CAPTURE-SITE
               PERFORM 4030-LIMIT-STORAGE
               PERFORM 4040-SPLIT-STORAGE
               PERFORM 4050-PRORATE-RETRIEVE-FEE
               PERFORM 4060-EDIT-REJECT
               PERFORM 4070-COUNT-EXCEPT
               PERFORM 4080-DESCRIBE-TRAN
               PERFORM 4090-DESCRIBE-ITEM-TYPE
               PERFORM 4100-PRORATE-RETRIEVE-FEE
               PERFORM 4110-AGE-NEXT-REVIEW
               PERFORM 4120-DESCRIBE-ITEM-TYPE
               PERFORM 4130-EDIT-OPERATOR
               PERFORM 4140-ASSESS-REVERSAL
               PERFORM 4150-AGE-MATURITY
               PERFORM 4160-PRORATE-STORAGE
               PERFORM 7000-CALL-CIRISK
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
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-RETRY.
           IF CIMM-RETRY-CNT < 9999999
               ADD 1 TO CIMM-RETRY-CNT
           END-IF
           IF CIMM-RETRY-CNT NOT < 12
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3030-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD < 0.00
               MOVE 0.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 2500.00
               MOVE 2500.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3040-FLAG-ITEM.
           IF CIMM-ITEM-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-ITEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT < 50.00
               MOVE 50.00 TO CIMM-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-RETRIEVE-FEE-AMT > 10000.00
               MOVE 10000.00 TO CIMM-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN POST DATE IS PAST
      * JY -> RU WHEN POST DATE IS PAST
      * RU -> UR WHEN POST DATE IS PAST
      * UR -> YW WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YW' TO CIMM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E065 CUST ID REQUIRED
      * E103 IMAGE FEE AMT NEGATIVE
      * E054 IMAGE FEE AMT OVER 250000
      * E621 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CUST-ID = SPACES
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN CIMT-IMAGE-FEE-AMT < 0
                   MOVE 'E103' TO WS-REJECT-CODE
               WHEN CIMT-IMAGE-FEE-AMT > 250000.00
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-TYPE-CD
                   ADD CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-MATURITY.
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
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMT-ITEM-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIMT-ITEM-AMT - (WS-WORK-AMT * 4)
           COMPUTE CIMM-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-RETRY.
           IF CIMM-RETRY-CNT < 9999999
               ADD 1 TO CIMM-RETRY-CNT
           END-IF
           IF CIMM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 100.00
               MOVE 100.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 5000.00
               MOVE 5000.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-RETRY.
           IF CIMM-RETRY-CNT < 9999999
               ADD 1 TO CIMM-RETRY-CNT
           END-IF
           IF CIMM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-ITEM-TYPE-CD
                   ADD CIMT-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-ITEM-BAND-LOW(WS-TX)
                   MOVE WS-ITEM-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-ITEM.
           IF CIMT-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-ITEM-AMT * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-OPEN.
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
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM BAND TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CIMM-RETRIEVE-FEE-AMT NOT < WS-ITEM-BAND-LOW(WS-TX)
                   MOVE WS-ITEM-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-ITEM.
           IF CIMT-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-ITEM-AMT * CIMM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CIMM-LAST-MAINT-DATE > 0 AND CIMM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMT-RETRIEVE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMT-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 3)
           COMPUTE CIMM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE IMAGE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CIMM-IMAGE-FEE-YTD NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE SCHED TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-SCHED-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CIMT-ITEM-AMT NOT < WS-IMAGE-FE-SCHED-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-SCHED-RATE(WS-TX) TO
                       CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMT-TRAN-CD
                   ADD CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E642 DESC REQUIRED
      * E154 WORK AMT NEGATIVE
      * E456 WORK AMT OVER 25000
      * E649 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-DESC = SPACES
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN CIMM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE YTD FEE ON THE ITEM YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3280-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = CIMM-ITEM-YTD *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMM-ITEM-TYPE-CD = 'CW'
                   MOVE 20.00 TO CIMM-IMAGE-FEE-YTD
               WHEN CIMM-ITEM-TYPE-CD = 'FB'
                   MOVE 3.00 TO CIMM-IMAGE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = CIMM-ITEM-YTD *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-YTD > 0 AND CIMM-IMAGE-FEE-YTD < 10.00
               MOVE 10.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 25.00
               MOVE 25.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-AMT * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE CIMT-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE STORAGE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3320-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIMM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED = CIMT-STORAGE-AMT
                       * CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 7.00 TO CIMM-IMAGE-FEE-AMT
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 12.00 TO CIMM-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED = CIMT-STORAGE-AMT
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-AMT > 0 AND CIMM-IMAGE-FEE-AMT < 10.00
               MOVE 10.00 TO CIMM-IMAGE-FEE-AMT
           END-IF
           IF CIMM-IMAGE-FEE-AMT > 75.00
               MOVE 75.00 TO CIMM-IMAGE-FEE-AMT
           END-IF
           ADD CIMM-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ITEM AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3330-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMM-ITEM-AMT *
                       CIMM-FEE-PCT / 200
               WHEN CIMT-TRAN-CD = 'CP'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN CIMT-TRAN-CD = 'RT'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMM-ITEM-AMT *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-OPEN.
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
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E623 OPERATOR REQUIRED
      * E724 STORAGE AMT NEGATIVE
      * E918 STORAGE AMT OVER 99999
      * E154 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-OPERATOR = SPACES
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN CIMT-STORAGE-AMT < 0
                   MOVE 'E724' TO WS-REJECT-CODE
               WHEN CIMT-STORAGE-AMT > 99999.00
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-ITEM.
           IF CIMT-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-ITEM-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN NEXT DUE DATE IS PAST
      * FB -> HN WHEN NEXT DUE DATE IS PAST
      * HN -> KX WHEN NEXT DUE DATE IS PAST
      * KX -> CW WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN EFF DATE IS PAST
      * FB -> HN WHEN EFF DATE IS PAST
      * HN -> KX WHEN EFF DATE IS PAST
      * KX -> CW WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-AMT ROUNDED =
                   CIMT-ITEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3420-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMM-STORAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-STORAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMT-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3450-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CP -> RT WHEN POST DATE IS PAST
      * RT -> ST WHEN POST DATE IS PAST
      * ST -> PG WHEN POST DATE IS PAST
      * PG -> CP WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF CIMT-POST-DATE > 0 AND CIMT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E482 NAME REQUIRED
      * E893 RETRIEVE FEE AMT NEGATIVE
      * E806 RETRIEVE FEE AMT OVER 5000
      * E351 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-NAME = SPACES
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-AMT < 0
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-AMT > 5000.00
                   MOVE 'E806' TO WS-REJECT-CODE
               WHEN CIMM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMM-STORAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRIEVE FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3520-LIMIT-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-YTD < 0.00
               MOVE 0.00 TO CIMM-RETRIEVE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-RETRIEVE-FEE-YTD > 2500.00
               MOVE 2500.00 TO CIMM-RETRIEVE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-STORAGE.
           IF CIMT-STORAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-LAST-STMT.
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
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-BAND-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-ITEM-BAND-LOW(WS-TX)
                   MOVE WS-ITEM-BAND-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE ITEM YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3570-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIMM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CIMM-ITEM-YTD *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMM-BRANCH-CD = 'DJ'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN CIMM-BRANCH-CD = 'FX'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CIMM-ITEM-YTD *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E499 DESC REQUIRED
      * E416 STORAGE AMT NEGATIVE
      * E006 STORAGE AMT OVER 99999
      * E759 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-DESC = SPACES
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT < 0
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT > 99999.00
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN CIMM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE CIMM-RETRIEVE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE IMAGE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3600-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMT-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                       CIMM-IMAGE-FEE-YTD * CIMM-PENALTY-PCT / 200
               WHEN CIMT-TRAN-CD = 'CP'
                   MOVE 12.00 TO CIMT-IMAGE-FEE-AMT
               WHEN CIMT-TRAN-CD = 'RT'
                   MOVE 12.00 TO CIMT-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                       CIMM-IMAGE-FEE-YTD * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMT-IMAGE-FEE-AMT > 0 AND CIMT-IMAGE-FEE-AMT < 10.00
               MOVE 10.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 100.00
               MOVE 100.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-BRANCH.
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
      * ACCUMULATE STORAGE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-STATUS-CD
                   ADD CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CIMM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE STORAGE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3640-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIMM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMT-STORAGE-AMT *
                       CIMM-FEE-PCT / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 15.00 TO CIMM-STORAGE-AMT
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 20.00 TO CIMM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMT-STORAGE-AMT *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-STORAGE-AMT > 0 AND CIMM-STORAGE-AMT < 5.00
               MOVE 5.00 TO CIMM-STORAGE-AMT
           END-IF
           IF CIMM-STORAGE-AMT > 150.00
               MOVE 150.00 TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE SCHED TIER FOR THE RETRIEVE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-SCHED-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CIMM-RETRIEVE-FEE-YTD NOT <
                   WS-IMAGE-FE-SCHED-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-SCHED-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3660-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE STORAGE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3670-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMT-STORAGE-AMT *
                       CIMM-FEE-PCT / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 20.00 TO CIMM-ITEM-AMT
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 20.00 TO CIMM-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMT-STORAGE-AMT *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-ITEM-AMT > 0 AND CIMM-ITEM-AMT < 1.00
               MOVE 1.00 TO CIMM-ITEM-AMT
           END-IF
           IF CIMM-ITEM-AMT > 75.00
               MOVE 75.00 TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EFFECTIVE.
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
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED =
                   CIMM-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMM-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO CIMM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-LAST-STMT.
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
               WHEN WS-DAYS NOT > 60
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3740-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-YTD ROUNDED = WS-WORK-AMT-2 *
                       CIMM-FEE-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO CIMM-STORAGE-YTD
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO CIMM-STORAGE-YTD
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-YTD ROUNDED = WS-WORK-AMT-2 *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-STORAGE-YTD > 0 AND CIMM-STORAGE-YTD < 5.00
               MOVE 5.00 TO CIMM-STORAGE-YTD
           END-IF
           IF CIMM-STORAGE-YTD > 250.00
               MOVE 250.00 TO CIMM-STORAGE-YTD
           END-IF
           ADD CIMM-STORAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-OPEN.
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
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-MATURITY.
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
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-MATURITY.
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
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 10.00
               MOVE 10.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 10000.00
               MOVE 10000.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 0.00
               MOVE 0.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 50000.00
               MOVE 50000.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RETRIEVE FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3830-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMT-RETRIEVE-FEE-AMT
                       * CIMM-PENALTY-PCT / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMT-RETRIEVE-FEE-AMT
                       * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ITEM BAND TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CIMM-ITEM-AMT NOT < WS-ITEM-BAND-LOW(WS-TX)
                   MOVE WS-ITEM-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-MATURITY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3870-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       3880-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E975 DESC REQUIRED
      * E222 WORK AMT NEGATIVE
      * E429 WORK AMT OVER 99999
      * E493 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-DESC = SPACES
                   MOVE 'E975' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE CIMT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3940-FLAG-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMM-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM YTD FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3950-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-YTD ROUNDED = CIMT-IMAGE-FEE-AMT *
                       CIMM-FEE-PCT / 200
               WHEN CIMT-TRAN-CD = 'CP'
                   MOVE 5.00 TO CIMM-ITEM-YTD
               WHEN CIMT-TRAN-CD = 'RT'
                   MOVE 3.00 TO CIMM-ITEM-YTD
               WHEN OTHER
                   COMPUTE CIMM-ITEM-YTD ROUNDED = CIMT-IMAGE-FEE-AMT *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-ITEM-YTD > 0 AND CIMM-ITEM-YTD < 2.00
               MOVE 2.00 TO CIMM-ITEM-YTD
           END-IF
           IF CIMM-ITEM-YTD > 25.00
               MOVE 25.00 TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-STORAGE.
           IF CIMT-STORAGE-AMT < 50.00
               MOVE 50.00 TO CIMT-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-STORAGE-AMT > 50000.00
               MOVE 50000.00 TO CIMT-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM BAND TIER FOR THE IMAGE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-BAND-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CIMM-IMAGE-FEE-AMT NOT < WS-ITEM-BAND-LOW(WS-TX)
                   MOVE WS-ITEM-BAND-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3980-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                       CIMM-IMAGE-FEE-AMT * CIMM-PENALTY-PCT / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 3.00 TO CIMT-RETRIEVE-FEE-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 12.00 TO CIMT-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                       CIMM-IMAGE-FEE-AMT * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMT-RETRIEVE-FEE-AMT > 0 AND CIMT-RETRIEVE-FEE-AMT < 2.00
               MOVE 2.00 TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           IF CIMT-RETRIEVE-FEE-AMT > 150.00
               MOVE 150.00 TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-OPEN.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E078 ADDR REQUIRED
      * E501 ITEM AMT NEGATIVE
      * E891 ITEM AMT OVER 99999
      * E056 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT < 0
                   MOVE 'E501' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT > 99999.00
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN CIMM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE RETRIEVE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4010-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIMT-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMT-ITEM-AMT ROUNDED = CIMM-RETRIEVE-FEE-YTD
                       * CIMM-PENALTY-PCT / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 7.00 TO CIMT-ITEM-AMT
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 7.00 TO CIMT-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMT-ITEM-AMT ROUNDED = CIMM-RETRIEVE-FEE-YTD
                       * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMT-ITEM-AMT > 0 AND CIMT-ITEM-AMT < 1.00
               MOVE 1.00 TO CIMT-ITEM-AMT
           END-IF
           IF CIMT-ITEM-AMT > 75.00
               MOVE 75.00 TO CIMT-ITEM-AMT
           END-IF
           ADD CIMT-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY CAPTURE SITE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-CAPTURE-SITE.
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
      * STORAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4030-LIMIT-STORAGE.
           IF CIMM-STORAGE-AMT < 0.00
               MOVE 0.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-AMT > 500.00
               MOVE 500.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMT-STORAGE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMT-STORAGE-AMT - (WS-WORK-AMT * 3)
           COMPUTE CIMM-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMM-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E027 REJECT CD REQUIRED
      * E821 ITEM AMT NEGATIVE
      * E318 ITEM AMT OVER 5000
      * E174 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-REJECT-CD = SPACES
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT < 0
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT > 5000.00
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN CIMM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E174' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ST'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       4100-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMT-ITEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-NEXT-REVIEW.
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
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E750 OPERATOR REQUIRED
      * E671 RETRIEVE FEE AMT NEGATIVE
      * E376 RETRIEVE FEE AMT OVER 99999
      * E317 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-OPERATOR = SPACES
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN CIMT-RETRIEVE-FEE-AMT < 0
                   MOVE 'E671' TO WS-REJECT-CODE
               WHEN CIMT-RETRIEVE-FEE-AMT > 99999.00
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN CIMM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4140-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                       CIMT-IMAGE-FEE-AMT * CIMM-FEE-PCT / 200
               WHEN CIMM-ITEM-TYPE-CD = 'CW'
                   MOVE 15.00 TO CIMT-RETRIEVE-FEE-AMT
               WHEN CIMM-ITEM-TYPE-CD = 'FB'
                   MOVE 3.00 TO CIMT-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                       CIMT-IMAGE-FEE-AMT * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMT-RETRIEVE-FEE-AMT > 0 AND CIMT-RETRIEVE-FEE-AMT < 1.00
               MOVE 1.00 TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           IF CIMT-RETRIEVE-FEE-AMT > 35.00
               MOVE 35.00 TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4150-AGE-MATURITY.
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
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       4160-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

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
                   MOVE CIRISK-IMAGE-FEE-AMT
                     TO CIMM-IMAGE-FEE-AMT
                   MOVE CIRISK-STORAGE-YTD
                     TO CIMM-STORAGE-YTD
                   MOVE CIRISK-RETRIEVE-FEE-YTD
                     TO CIMM-RETRIEVE-FEE-YTD
                   MOVE CIRISK-ITEM-YTD
                     TO CIMM-ITEM-YTD
                   MOVE CIRISK-REVIEW-FLAG
                     TO CIMM-REVIEW-FLAG
                   MOVE CIRISK-HOLD-FLAG
                     TO CIMM-HOLD-FLAG
                   MOVE CIRISK-OVERRIDE-FLAG
                     TO CIMM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-CIRISK' TO ABND-PARAGRAPH
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
           DISPLAY 'CIMPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CIMPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
