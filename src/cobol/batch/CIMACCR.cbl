      *================================================================*
      * PROGRAM  : CIMACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHECK IMAGE ARCHIVE - DAILY ACCRUAL. READS THE CIM  *
      *            TRANSACTIONS, UPDATES THE CIM MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CHECK IMAGE CAPTURE INDEX,      *
      *            RETRIEVAL AND IMAGE STATEMENT FEES                  *
      * INPUT    : CIMMST (KSDS), CIMTRN, BANKCTL                      *
      * OUTPUT   : CIMOUT, CIMACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CIRISK, CIVAL           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-27 EWS  ORIGINAL                                       *
      * 1994-03-11 RJT  STORAGE STATUS CODES ADDED PRB5185             *
      * 1994-03-28 CRP  ITEM CALCULATION CORRECTED INC8852             *
      * 1994-09-01 TWH  NEW STORAGE REPORT COLUMNS INC3246             *
      * 1995-11-28 PQR  STORAGE STATUS CODES ADDED REQ9056             *
      * 1997-09-16 LMB  ITEM LIMIT REVISED REQ6132                     *
      * 1999-05-15 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-09-06 LMB  ITEM RERUN PROTECTION INC2172                  *
      * 2009-08-08 EWS  PERFORMANCE - ITEM READ AHEAD PRB5266          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIMACCR.
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
           SELECT REPORT-FILE ASSIGN TO "CIMACRPT"
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
      * WS-ITEM-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000203000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000441500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000629500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000495000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002300000000414000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002400000000366500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004900000000229500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005400000000516000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007900000000026000'.
       01  WS-ITEM-TIER-TBL REDEFINES WS-ITEM-TIER-VALUES.
           05  WS-ITEM-TIER-ROW OCCURS 9 TIMES.
               10  WS-ITEM-TIER-LOW        PIC 9(09)V99.
               10  WS-ITEM-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000890500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000065500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000473000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000199500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001850000000769000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001900000000402000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011900000000752500'.
       01  WS-STORAGE-BAND-TBL REDEFINES WS-STORAGE-BAND-VALUES.
           05  WS-STORAGE-BAND-ROW OCCURS 7 TIMES.
               10  WS-STORAGE-BAND-LOW     PIC 9(09)V99.
               10  WS-STORAGE-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CIMACCR  CHECK IMAGE ARCHIVE'.
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
               PERFORM 3000-DESCRIBE-TRAN
               PERFORM 3010-CHECK-OPEN
               PERFORM 3020-SELECT-PENALTY-PCT
               PERFORM 3030-CHECK-MATURITY
               PERFORM 3040-PRORATE-IMAGE-FEE
               PERFORM 3050-SET-TRAN
               PERFORM 3060-DESCRIBE-ITEM-TYPE
               PERFORM 3070-AGE-CLOSE
               PERFORM 3080-ACCRUE-RETRIEVE-FEE
               PERFORM 3090-CHECK-NEXT-REVIEW
               PERFORM 3100-ACCRUE-PENALTY-PCT
               PERFORM 3110-FLAG-IMAGE-FEE
               PERFORM 3120-FLAG-ITEM
               PERFORM 3130-CHECK-EFFECTIVE
               PERFORM 3140-CHECK-NEXT-DUE
               PERFORM 3150-ACCUM-CHANNEL
               PERFORM 3160-LIMIT-STORAGE
               PERFORM 3170-SET-CHANNEL
               PERFORM 3180-COUNT-EXCEPT
               PERFORM 3190-DESCRIBE-BRANCH
               PERFORM 3200-PRORATE-RETRIEVE-FEE
               PERFORM 3210-CHECK-NOTICE
               PERFORM 3220-SELECT-MARGIN
               PERFORM 3230-SELECT-FEE-PCT
               PERFORM 3240-PRORATE-STORAGE
               PERFORM 3250-SET-CHANNEL
               PERFORM 3260-FLAG-RETRIEVE-FEE
               PERFORM 3270-SPLIT-IMAGE-FEE
               PERFORM 3280-DESCRIBE-BRANCH
               PERFORM 3290-SET-ITEM-TYPE
               PERFORM 3300-SPLIT-IMAGE-FEE
               PERFORM 3310-ASSESS-SUSPEND
               PERFORM 3320-ACCRUE-STORAGE
               PERFORM 3330-DESCRIBE-CAPTURE-SITE
               PERFORM 3340-COUNT-EXCEPT
               PERFORM 3350-ACCUM-ITEM-TYPE
               PERFORM 3360-SELECT-PENALTY-PCT
               PERFORM 3370-ACCUM-TRAN
               PERFORM 3380-SPLIT-ITEM
               PERFORM 3390-ACCRUE-RETRIEVE-FEE
               PERFORM 3400-SELECT-FEE-PCT
               PERFORM 3410-ASSESS-HOLD
               PERFORM 3420-PRORATE-ITEM
               PERFORM 3430-SELECT-MARGIN
               PERFORM 3440-COUNT-NOTICE
               PERFORM 3450-COUNT-SEQ
               PERFORM 3460-SPLIT-ITEM
               PERFORM 3470-ASSESS-REVIEW
               PERFORM 3480-SPLIT-RETRIEVE-FEE
               PERFORM 3490-LIMIT-ITEM
               PERFORM 3500-SELECT-FEE-PCT
               PERFORM 3510-DESCRIBE-ITEM-TYPE
               PERFORM 3520-PRORATE-RETRIEVE-FEE
               PERFORM 3530-SPLIT-RETRIEVE-FEE
               PERFORM 3540-ASSESS-PREFERRED
               PERFORM 3550-SELECT-PENALTY-PCT
               PERFORM 3560-FLAG-STORAGE
               PERFORM 3570-CHECK-NOTICE
               PERFORM 3580-DESCRIBE-ITEM-TYPE
               PERFORM 3590-DESCRIBE-CHANNEL
               PERFORM 3600-SPLIT-RETRIEVE-FEE
               PERFORM 3610-SPLIT-ITEM
               PERFORM 3620-EDIT-OPERATOR
               PERFORM 3630-SPLIT-STORAGE
               PERFORM 3640-SPLIT-ITEM
               PERFORM 3650-EDIT-MEMO
               PERFORM 3660-SPLIT-RETRIEVE-FEE
               PERFORM 3670-AGE-MATURITY
               PERFORM 3680-FLAG-STORAGE
               PERFORM 3690-FLAG-ITEM
               PERFORM 3700-SET-BRANCH
               PERFORM 3710-DESCRIBE-ITEM-TYPE
               PERFORM 3720-ASSESS-OVERRIDE
               PERFORM 3730-PRORATE-ITEM
               PERFORM 3740-AGE-CLOSE
               PERFORM 3750-ASSESS-PREFERRED
               PERFORM 3760-EDIT-CUST-ID
               PERFORM 3770-FLAG-ITEM
               PERFORM 3780-SPLIT-IMAGE-FEE
               PERFORM 3790-SPLIT-STORAGE
               PERFORM 3800-AGE-CLOSE
               PERFORM 3810-EDIT-DESC
               PERFORM 3820-AGE-LAST-STMT
               PERFORM 3830-COUNT-EXCEPT
               PERFORM 3840-EDIT-ADDR
               PERFORM 3850-PRORATE-ITEM
               PERFORM 3860-AGE-NEXT-DUE
               PERFORM 3870-EDIT-OPERATOR
               PERFORM 3880-ACCUM-CAPTURE-SITE
               PERFORM 3890-ASSESS-SUSPEND
               PERFORM 3900-PRORATE-ITEM
               PERFORM 3910-SELECT-MARGIN
               PERFORM 3920-ACCUM-ITEM-TYPE
               PERFORM 3930-SPLIT-ITEM
               PERFORM 3940-SPLIT-RETRIEVE-FEE
               PERFORM 3950-COUNT-SEQ
               PERFORM 3960-FLAG-ITEM
               PERFORM 3970-FLAG-ITEM
               PERFORM 3980-SELECT-PENALTY-PCT
               PERFORM 3990-SPLIT-ITEM
               PERFORM 4000-SET-TRAN
               PERFORM 4010-EDIT-MEMO
               PERFORM 4020-CHECK-NEXT-DUE
               PERFORM 4030-COUNT-NOTICE
               PERFORM 4040-EDIT-REF-NO
               PERFORM 4050-SELECT-MARGIN
               PERFORM 4060-PRORATE-STORAGE
               PERFORM 4070-LIMIT-STORAGE
               PERFORM 4080-FLAG-STORAGE
               PERFORM 4090-EDIT-REJECT
               PERFORM 4100-CHECK-OPEN
               PERFORM 4110-SELECT-FEE-PCT
               PERFORM 4120-PRORATE-IMAGE-FEE
               PERFORM 4130-DESCRIBE-TYPE
               PERFORM 4140-PRORATE-RETRIEVE-FEE
               PERFORM 7000-CALL-CIRISK
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
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ST'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-OPEN.
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
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM TIER TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-TIER-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CIMT-ITEM-AMT NOT < WS-ITEM-TIER-LOW(WS-TX)
                   MOVE WS-ITEM-TIER-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-MATURITY.
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
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-AMT
           END-IF
           ADD CIMM-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CP -> RT WHEN LAST MAINT DATE IS PAST
      * RT -> ST WHEN LAST MAINT DATE IS PAST
      * ST -> PG WHEN LAST MAINT DATE IS PAST
      * PG -> CP WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   IF CIMM-LAST-MAINT-DATE > 0 AND CIMM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RT' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF CIMM-LAST-MAINT-DATE > 0 AND CIMM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ST' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF CIMM-LAST-MAINT-DATE > 0 AND CIMM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PG' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF CIMM-LAST-MAINT-DATE > 0 AND CIMM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CP' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-CLOSE.
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
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-YTD * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CIMM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-NEXT-REVIEW-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3100-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3110-FLAG-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3120-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-CHANNEL.
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
      * STORAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3160-LIMIT-STORAGE.
           IF CIMM-STORAGE-YTD < 0.00
               MOVE 0.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-YTD > 500.00
               MOVE 500.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3200-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-NOTICE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMM-RETRIEVE-FEE-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-STORAGE-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-CHANNEL.
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
      * RETRIEVE FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3260-FLAG-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMM-RETRIEVE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3270-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE CIMT-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN OPEN DATE IS PAST
      * FB -> HN WHEN OPEN DATE IS PAST
      * HN -> KX WHEN OPEN DATE IS PAST
      * KX -> CW WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMT-IMAGE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIMT-IMAGE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE CIMM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3310-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIMT-STORAGE-AMT ROUNDED = WS-WORK-AMT-2 *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO CIMT-STORAGE-AMT
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO CIMT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMT-STORAGE-AMT ROUNDED = WS-WORK-AMT-2 *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMT-STORAGE-AMT > 0 AND CIMT-STORAGE-AMT < 1.00
               MOVE 1.00 TO CIMT-STORAGE-AMT
           END-IF
           IF CIMT-STORAGE-AMT > 75.00
               MOVE 75.00 TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-STORAGE.
           IF CIMT-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-STORAGE-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE AMT BY ITEM TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-ITEM-TYPE-CD
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMM-STORAGE-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMT-TRAN-CD
                   ADD CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMT-ITEM-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMT-ITEM-AMT - (WS-WORK-AMT * 3)
           COMPUTE CIMM-IMAGE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMM-ITEM-YTD NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE ITEM AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3410-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIMM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CIMM-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED = CIMT-ITEM-AMT
                       * CIMM-MARGIN / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 15.00 TO CIMM-RETRIEVE-FEE-AMT
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 7.00 TO CIMM-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED = CIMT-ITEM-AMT
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-RETRIEVE-FEE-AMT > 0 AND CIMM-RETRIEVE-FEE-AMT <
               10.00
               MOVE 10.00 TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           IF CIMM-RETRIEVE-FEE-AMT > 100.00
               MOVE 100.00 TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMM-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       3420-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-YTD ROUNDED =
                   CIMM-IMAGE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ITEM TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3430-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-TIER-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-ITEM-TIER-LOW(WS-TX)
                   MOVE WS-ITEM-TIER-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 10
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-AMT - (WS-WORK-AMT * 3)
           COMPUTE CIMM-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE YTD FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3470-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                       CIMT-STORAGE-AMT * CIMM-FEE-PCT / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 12.00 TO CIMM-RETRIEVE-FEE-YTD
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 3.00 TO CIMM-RETRIEVE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                       CIMT-STORAGE-AMT * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-RETRIEVE-FEE-YTD > 0 AND CIMM-RETRIEVE-FEE-YTD < 2.00
               MOVE 2.00 TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           IF CIMM-RETRIEVE-FEE-YTD > 50.00
               MOVE 50.00 TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE CIMT-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3490-LIMIT-ITEM.
           IF CIMM-ITEM-AMT < 25.00
               MOVE 25.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-AMT > 10000.00
               MOVE 10000.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3500-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMM-ITEM-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       3520-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMT-RETRIEVE-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIMT-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 12)
           COMPUTE CIMM-RETRIEVE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE ITEM AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3540-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CIMT-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED = CIMT-ITEM-AMT *
                       CIMM-MARGIN / 200
               WHEN CIMM-ITEM-TYPE-CD = 'CW'
                   MOVE 5.00 TO CIMT-IMAGE-FEE-AMT
               WHEN CIMM-ITEM-TYPE-CD = 'FB'
                   MOVE 5.00 TO CIMT-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED = CIMT-ITEM-AMT *
                       CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMT-IMAGE-FEE-AMT > 0 AND CIMT-IMAGE-FEE-AMT < 10.00
               MOVE 10.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 75.00
               MOVE 75.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMT-STORAGE-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3560-FLAG-STORAGE.
           IF CIMT-STORAGE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-NOTICE.
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
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-RETRIEVE-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-RETRIEVE-FEE-YTD - (WS-WORK-AMT
               * 4)
           COMPUTE CIMM-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-AMT - (WS-WORK-AMT * 2)
           COMPUTE CIMT-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E402 OPERATOR REQUIRED
      * E798 ITEM AMT NEGATIVE
      * E984 ITEM AMT OVER 50000
      * E934 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-OPERATOR = SPACES
                   MOVE 'E402' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT < 0
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT > 50000.00
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN CIMM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMM-STORAGE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-STORAGE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CIMM-RETRIEVE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-YTD - (WS-WORK-AMT * 3)
           COMPUTE CIMM-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E659 MEMO REQUIRED
      * E693 ITEM AMT NEGATIVE
      * E242 ITEM AMT OVER 10000
      * E923 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-MEMO = SPACES
                   MOVE 'E659' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-AMT < 0
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-AMT > 10000.00
                   MOVE 'E242' TO WS-REJECT-CODE
               WHEN CIMM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E923' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-RETRIEVE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CIMM-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 2)
           COMPUTE CIMM-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-MATURITY.
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
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3680-FLAG-STORAGE.
           IF CIMM-STORAGE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3690-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN LAST STMT DATE IS PAST
      * FX -> MT WHEN LAST STMT DATE IS PAST
      * MT -> UA WHEN LAST STMT DATE IS PAST
      * UA -> VL WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-BRANCH.
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
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3710-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3720-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED = WS-WORK-AMT-2
                       * CIMM-FEE-PCT / 200
               WHEN CIMT-TRAN-CD = 'CP'
                   MOVE 3.00 TO CIMT-RETRIEVE-FEE-AMT
               WHEN CIMT-TRAN-CD = 'RT'
                   MOVE 5.00 TO CIMT-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED = WS-WORK-AMT-2
                       * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMT-RETRIEVE-FEE-AMT > 0 AND CIMT-RETRIEVE-FEE-AMT < 1.00
               MOVE 1.00 TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           IF CIMT-RETRIEVE-FEE-AMT > 250.00
               MOVE 250.00 TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3730-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-CLOSE.
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
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE ITEM AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3750-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMM-ITEM-AMT *
                       CIMM-MARGIN / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 15.00 TO CIMM-STORAGE-AMT
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 5.00 TO CIMM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMM-ITEM-AMT *
                       CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-STORAGE-AMT > 0 AND CIMM-STORAGE-AMT < 10.00
               MOVE 10.00 TO CIMM-STORAGE-AMT
           END-IF
           IF CIMM-STORAGE-AMT > 150.00
               MOVE 150.00 TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E945 CUST ID REQUIRED
      * E701 ITEM AMT NEGATIVE
      * E176 ITEM AMT OVER 25000
      * E520 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CUST-ID = SPACES
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT < 0
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT > 25000.00
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN CIMT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3770-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE CIMM-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMM-STORAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CIMM-STORAGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE CIMT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-CLOSE.
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
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E257 DESC REQUIRED
      * E069 STORAGE AMT NEGATIVE
      * E635 STORAGE AMT OVER 250000
      * E805 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-DESC = SPACES
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN CIMT-STORAGE-AMT < 0
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN CIMT-STORAGE-AMT > 250000.00
                   MOVE 'E635' TO WS-REJECT-CODE
               WHEN CIMM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-LAST-STMT.
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
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E146 ADDR REQUIRED
      * E528 WORK AMT NEGATIVE
      * E416 WORK AMT OVER 25000
      * E880 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN CIMM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E880' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3850-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CIMM-ITEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
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
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E226 OPERATOR REQUIRED
      * E085 ITEM AMT NEGATIVE
      * E137 ITEM AMT OVER 5000
      * E229 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-OPERATOR = SPACES
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT < 0
                   MOVE 'E085' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT > 5000.00
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN CIMM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CAPTURE-SITE-CD
                   ADD CIMT-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3890-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMM-IMAGE-FEE-AMT *
                       CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 12.00 TO CIMM-ITEM-AMT
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 5.00 TO CIMM-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMM-IMAGE-FEE-AMT *
                       CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-ITEM-AMT > 0 AND CIMM-ITEM-AMT < 10.00
               MOVE 10.00 TO CIMM-ITEM-AMT
           END-IF
           IF CIMM-ITEM-AMT > 50.00
               MOVE 50.00 TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       3900-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-YTD ROUNDED =
                   CIMM-STORAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE IMAGE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
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
      * ACCUMULATE RETRIEVE FEE AMT BY ITEM TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-ITEM-TYPE-CD
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE CIMT-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-RETRIEVE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-RETRIEVE-FEE-YTD - (WS-WORK-AMT
               * 12)
           COMPUTE CIMT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 5
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3960-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3970-FLAG-ITEM.
           IF CIMT-ITEM-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-ITEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMM-RETRIEVE-FEE-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CP -> RT WHEN OPEN DATE IS PAST
      * RT -> ST WHEN OPEN DATE IS PAST
      * ST -> PG WHEN OPEN DATE IS PAST
      * PG -> CP WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF CIMM-OPEN-DATE > 0 AND CIMM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO CIMT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E213 MEMO REQUIRED
      * E150 WORK AMT 2 NEGATIVE
      * E352 WORK AMT 2 OVER 250000
      * E393 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-MEMO = SPACES
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E150' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E202 REF NO REQUIRED
      * E569 RETRIEVE FEE AMT NEGATIVE
      * E718 RETRIEVE FEE AMT OVER 5000
      * E945 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-REF-NO = SPACES
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN CIMT-RETRIEVE-FEE-AMT < 0
                   MOVE 'E569' TO WS-REJECT-CODE
               WHEN CIMT-RETRIEVE-FEE-AMT > 5000.00
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN CIMM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMT-RETRIEVE-FEE-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   CIMM-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4070-LIMIT-STORAGE.
           IF CIMM-STORAGE-YTD < 0.00
               MOVE 0.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-YTD > 10000.00
               MOVE 10000.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4080-FLAG-STORAGE.
           IF CIMT-STORAGE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E487 REJECT CD REQUIRED
      * E687 ITEM YTD NEGATIVE
      * E931 ITEM YTD OVER 5000
      * E331 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-REJECT-CD = SPACES
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-YTD < 0
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-YTD > 5000.00
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-OPEN.
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
      * SELECT THE STORAGE BAND TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CIMM-RETRIEVE-FEE-AMT NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE YTD.
      *----------------------------------------------------------------*
       4120-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       4140-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

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
                   MOVE CIRISK-STORAGE-AMT
                     TO CIMM-STORAGE-AMT
                   MOVE CIRISK-ITEM-YTD
                     TO CIMM-ITEM-YTD
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
                   MOVE CIVAL-RETRIEVE-FEE-AMT
                     TO CIMM-RETRIEVE-FEE-AMT
                   MOVE CIVAL-STORAGE-AMT
                     TO CIMM-STORAGE-AMT
                   MOVE CIVAL-ITEM-AMT
                     TO CIMM-ITEM-AMT
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
           DISPLAY 'CIMACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CIMACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
