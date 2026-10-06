      *================================================================*
      * PROGRAM  : CIMEDIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHECK IMAGE ARCHIVE - INPUT EDIT. READS THE CIM     *
      *            TRANSACTIONS, UPDATES THE CIM MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CHECK IMAGE CAPTURE INDEX,      *
      *            RETRIEVAL AND IMAGE STATEMENT FEES                  *
      * INPUT    : CIMMST (KSDS), CIMTRN, BANKCTL                      *
      * OUTPUT   : CIMOUT, CIMEDRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CIVAL, CIRISK, CIPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-10-22 EWS  ORIGINAL                                       *
      * 1990-02-25 DPS  RETRIEVE-FEE CALCULATION CORRECTED CHG2685     *
      * 1992-08-28 LMB  RETRIEVE-FEE EXCEPTIONS TO OPS QUEUE PRB7818   *
      * 1992-06-10 TWH  STORAGE CUTOFF MOVED REQ6302                   *
      * 1992-04-11 PQR  ITEM LIMIT REVISED INC3908                     *
      * 2000-01-20 DPS  PERFORMANCE - ITEM READ AHEAD INC3962          *
      * 2002-02-07 EWS  PERFORMANCE - STORAGE READ AHEAD PRB6773       *
      * 2003-11-21 PQR  RETRIEVE-FEE TABLE EXPANDED REQ8396            *
      * 2005-11-15 BHS  RETRIEVE-FEE FEE SCHEDULE UPDATE REQ3487       *
      * 2006-06-20 PQR  STORAGE FEE SCHEDULE UPDATE PRB3592            *
      * 2006-02-26 CRP  STORAGE CUTOFF MOVED PRB1974                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIMEDIT.
       AUTHOR. J L MARSH.

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
           SELECT REPORT-FILE ASSIGN TO "CIMEDRPT"
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
                                           VALUE 16.000000.
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
      * WS-RETRIEVE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETRIEVE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000258000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000522000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000873000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000181000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000779000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000776000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000137500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000479000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018000000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000247000'.
       01  WS-RETRIEVE-GRID-TBL REDEFINES WS-RETRIEVE-GRID-VALUES.
           05  WS-RETRIEVE-GRID-ROW OCCURS 10 TIMES.
               10  WS-RETRIEVE-GRID-LOW    PIC 9(09)V99.
               10  WS-RETRIEVE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ITEM-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000326000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000576500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000316000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000532000'.
       01  WS-ITEM-GRID-TBL REDEFINES WS-ITEM-GRID-VALUES.
           05  WS-ITEM-GRID-ROW OCCURS 4 TIMES.
               10  WS-ITEM-GRID-LOW        PIC 9(09)V99.
               10  WS-ITEM-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ITEM-GRIDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-GRIDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000808500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000047500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000439500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000688500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000067000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000112000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000118000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009050000000263500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009300000000344500'.
       01  WS-ITEM-GRIDX-TBL REDEFINES WS-ITEM-GRIDX-VALUES.
           05  WS-ITEM-GRIDX-ROW OCCURS 9 TIMES.
               10  WS-ITEM-GRIDX-LOW       PIC 9(09)V99.
               10  WS-ITEM-GRIDX-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CIMEDIT  CHECK IMAGE ARCHIVE'.
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
       COPY CIRISKP.
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
               PERFORM 3000-ASSESS-OVERRIDE
               PERFORM 3010-ACCUM-STATUS
               PERFORM 3020-CHECK-LAST-STMT
               PERFORM 3030-DESCRIBE-CHANNEL
               PERFORM 3040-PRORATE-RETRIEVE-FEE
               PERFORM 3050-COUNT-NOTICE
               PERFORM 3060-EDIT-CONTACT
               PERFORM 3070-LIMIT-RETRIEVE-FEE
               PERFORM 3080-FLAG-STORAGE
               PERFORM 3090-SELECT-MARGIN
               PERFORM 3100-COUNT-PAYMENT
               PERFORM 3110-COUNT-EXCEPT
               PERFORM 3120-ACCRUE-IMAGE-FEE
               PERFORM 3130-AGE-EFFECTIVE
               PERFORM 3140-SELECT-MARGIN
               PERFORM 3150-SPLIT-IMAGE-FEE
               PERFORM 3160-ACCUM-CHANNEL
               PERFORM 3170-PRORATE-RETRIEVE-FEE
               PERFORM 3180-LIMIT-ITEM
               PERFORM 3190-ASSESS-REVIEW
               PERFORM 3200-FLAG-IMAGE-FEE
               PERFORM 3210-CHECK-NEXT-REVIEW
               PERFORM 3220-EDIT-REF-NO
               PERFORM 3230-PRORATE-IMAGE-FEE
               PERFORM 3240-CHECK-POST
               PERFORM 3250-FLAG-IMAGE-FEE
               PERFORM 3260-SELECT-MARGIN
               PERFORM 3270-ASSESS-OVERRIDE
               PERFORM 3280-ACCUM-BRANCH
               PERFORM 3290-ASSESS-REVIEW
               PERFORM 3300-COUNT-PAYMENT
               PERFORM 3310-DESCRIBE-CAPTURE-SITE
               PERFORM 3320-COUNT-EXCEPT
               PERFORM 3330-FLAG-RETRIEVE-FEE
               PERFORM 3340-ACCUM-STATUS
               PERFORM 3350-PRORATE-STORAGE
               PERFORM 3360-FLAG-ITEM
               PERFORM 3370-SPLIT-ITEM
               PERFORM 3380-PRORATE-ITEM
               PERFORM 3390-CHECK-NEXT-DUE
               PERFORM 3400-PRORATE-STORAGE
               PERFORM 3410-AGE-OPEN
               PERFORM 3420-CHECK-NEXT-DUE
               PERFORM 3430-PRORATE-IMAGE-FEE
               PERFORM 3440-LIMIT-ITEM
               PERFORM 3450-AGE-CLOSE
               PERFORM 3460-LIMIT-STORAGE
               PERFORM 3470-FLAG-STORAGE
               PERFORM 3480-DESCRIBE-BRANCH
               PERFORM 3490-AGE-OPEN
               PERFORM 3500-LIMIT-ITEM
               PERFORM 3510-ACCRUE-STORAGE
               PERFORM 3520-DESCRIBE-CHANNEL
               PERFORM 3530-AGE-OPEN
               PERFORM 3540-COUNT-EXCEPT
               PERFORM 3550-ASSESS-PREFERRED
               PERFORM 3560-AGE-EFFECTIVE
               PERFORM 3570-SELECT-PENALTY-PCT
               PERFORM 3580-COUNT-SEQ
               PERFORM 3590-ACCUM-CHANNEL
               PERFORM 3600-SPLIT-IMAGE-FEE
               PERFORM 3610-CHECK-MATURITY
               PERFORM 3620-PRORATE-STORAGE
               PERFORM 3630-ACCUM-CHANNEL
               PERFORM 3640-SPLIT-ITEM
               PERFORM 3650-LIMIT-ITEM
               PERFORM 3660-ACCUM-CAPTURE-SITE
               PERFORM 3670-ASSESS-REVIEW
               PERFORM 3680-FLAG-ITEM
               PERFORM 3690-ASSESS-OVERRIDE
               PERFORM 3700-CHECK-LAST-STMT
               PERFORM 3710-SET-CHANNEL
               PERFORM 3720-DESCRIBE-CAPTURE-SITE
               PERFORM 3730-CHECK-MATURITY
               PERFORM 3740-SET-ITEM-TYPE
               PERFORM 3750-SELECT-MARGIN
               PERFORM 3760-COUNT-SEQ
               PERFORM 3770-ACCRUE-ITEM
               PERFORM 3780-SELECT-PENALTY-PCT
               PERFORM 3790-ACCUM-CHANNEL
               PERFORM 3800-ASSESS-REVERSAL
               PERFORM 3810-SET-CAPTURE-SITE
               PERFORM 3820-ASSESS-SUSPEND
               PERFORM 3830-LIMIT-IMAGE-FEE
               PERFORM 3840-DESCRIBE-ITEM-TYPE
               PERFORM 3850-FLAG-STORAGE
               PERFORM 3860-PRORATE-IMAGE-FEE
               PERFORM 3870-SPLIT-STORAGE
               PERFORM 3880-PRORATE-RETRIEVE-FEE
               PERFORM 3890-COUNT-RETRY
               PERFORM 3900-SPLIT-ITEM
               PERFORM 3910-PRORATE-RETRIEVE-FEE
               PERFORM 3920-EDIT-REF-NO
               PERFORM 3930-SPLIT-ITEM
               PERFORM 3940-DESCRIBE-TYPE
               PERFORM 3950-PRORATE-RETRIEVE-FEE
               PERFORM 3960-ASSESS-OVERRIDE
               PERFORM 3970-COUNT-EXCEPT
               PERFORM 3980-LIMIT-IMAGE-FEE
               PERFORM 3990-AGE-POST
               PERFORM 4000-FLAG-STORAGE
               PERFORM 4010-ACCRUE-STORAGE
               PERFORM 4020-ACCUM-STATUS
               PERFORM 4030-ASSESS-EXEMPT
               PERFORM 4040-ASSESS-REVIEW
               PERFORM 4050-PRORATE-RETRIEVE-FEE
               PERFORM 4060-ACCRUE-ITEM
               PERFORM 4070-LIMIT-STORAGE
               PERFORM 4080-COUNT-PAYMENT
               PERFORM 4090-DESCRIBE-TRAN
               PERFORM 4100-SET-ITEM-TYPE
               PERFORM 4110-LIMIT-IMAGE-FEE
               PERFORM 4120-FLAG-STORAGE
               PERFORM 4130-AGE-CLOSE
               PERFORM 4140-PRORATE-ITEM
               PERFORM 4150-ACCRUE-IMAGE-FEE
               PERFORM 4160-SPLIT-ITEM
               PERFORM 4170-SPLIT-ITEM
               PERFORM 4180-SELECT-FEE-PCT
               PERFORM 4190-COUNT-RETRY
               PERFORM 4200-LIMIT-STORAGE
               PERFORM 4210-ASSESS-OVERRIDE
               PERFORM 4220-DESCRIBE-TRAN
               PERFORM 4230-PRORATE-ITEM
               PERFORM 7010-CALL-CIVAL
               PERFORM 7000-CALL-CIRISK
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
      * ASSESS THE IMAGE FEE FEE ON THE RETRIEVE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3000-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED =
                       CIMM-RETRIEVE-FEE-AMT * CIMM-PENALTY-PCT / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 20.00 TO CIMM-IMAGE-FEE-AMT
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 15.00 TO CIMM-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED =
                       CIMM-RETRIEVE-FEE-AMT * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-AMT > 0 AND CIMM-IMAGE-FEE-AMT < 5.00
               MOVE 5.00 TO CIMM-IMAGE-FEE-AMT
           END-IF
           IF CIMM-IMAGE-FEE-AMT > 35.00
               MOVE 35.00 TO CIMM-IMAGE-FEE-AMT
           END-IF
           ADD CIMM-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-STATUS-CD
                   ADD CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-STMT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMT-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E192 CONTACT REQUIRED
      * E524 RETRIEVE FEE YTD NEGATIVE
      * E788 RETRIEVE FEE YTD OVER 99999
      * E233 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CONTACT = SPACES
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-YTD < 0
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-YTD > 99999.00
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN CIMT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT < 10.00
               MOVE 10.00 TO CIMT-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-RETRIEVE-FEE-AMT > 50000.00
               MOVE 50000.00 TO CIMT-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3080-FLAG-STORAGE.
           IF CIMM-STORAGE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETRIEVE GRID TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETRIEVE-GRID-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIMM-ITEM-AMT NOT < WS-RETRIEVE-GRID-LOW(WS-TX)
                   MOVE WS-RETRIEVE-GRID-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-IMAGE-FEE-AMT * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-RETRIEVE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRIDX TIER FOR THE IMAGE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRIDX-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CIMM-IMAGE-FEE-AMT NOT < WS-ITEM-GRIDX-LOW(WS-TX)
                   MOVE WS-ITEM-GRIDX-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-YTD - (WS-WORK-AMT *
               3)
           COMPUTE CIMM-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CHANNEL-CD
                   ADD CIMT-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED =
                   CIMM-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMM-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3190-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = WS-WORK-AMT-2 *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO CIMM-STORAGE-AMT
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO CIMM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = WS-WORK-AMT-2 *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-STORAGE-AMT > 0 AND CIMM-STORAGE-AMT < 1.00
               MOVE 1.00 TO CIMM-STORAGE-AMT
           END-IF
           IF CIMM-STORAGE-AMT > 50.00
               MOVE 50.00 TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3200-FLAG-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E596 REF NO REQUIRED
      * E374 RETRIEVE FEE AMT NEGATIVE
      * E259 RETRIEVE FEE AMT OVER 25000
      * E593 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-REF-NO = SPACES
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-AMT < 0
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN CIMM-RETRIEVE-FEE-AMT > 25000.00
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN CIMM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO IMAGE FEE YTD.
      *----------------------------------------------------------------*
       3230-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                   CIMT-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-POST.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3250-FLAG-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETRIEVE GRID TIER FOR THE IMAGE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETRIEVE-GRID-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIMM-IMAGE-FEE-YTD NOT < WS-RETRIEVE-GRID-LOW(WS-TX)
                   MOVE WS-RETRIEVE-GRID-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE YTD FEE ON THE STORAGE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3270-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                       CIMT-STORAGE-AMT * CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 15.00 TO CIMM-RETRIEVE-FEE-YTD
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 5.00 TO CIMM-RETRIEVE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                       CIMT-STORAGE-AMT * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-RETRIEVE-FEE-YTD > 0 AND CIMM-RETRIEVE-FEE-YTD < 2.00
               MOVE 2.00 TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           IF CIMM-RETRIEVE-FEE-YTD > 75.00
               MOVE 75.00 TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-BRANCH.
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
      * ASSESS THE STORAGE YTD FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3290-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-YTD ROUNDED = CIMT-IMAGE-FEE-AMT
                       * CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 15.00 TO CIMM-STORAGE-YTD
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 15.00 TO CIMM-STORAGE-YTD
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-YTD ROUNDED = CIMT-IMAGE-FEE-AMT
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-STORAGE-YTD > 0 AND CIMM-STORAGE-YTD < 10.00
               MOVE 10.00 TO CIMM-STORAGE-YTD
           END-IF
           IF CIMM-STORAGE-YTD > 100.00
               MOVE 100.00 TO CIMM-STORAGE-YTD
           END-IF
           ADD CIMM-STORAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3330-FLAG-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3340-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-STATUS-CD
                   ADD CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3350-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-STORAGE-AMT ROUNDED =
                   CIMM-STORAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3360-FLAG-ITEM.
           IF CIMT-ITEM-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMT-ITEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE CIMM-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       3380-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-YTD ROUNDED =
                   CIMT-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-OPEN.
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
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO IMAGE FEE YTD.
      *----------------------------------------------------------------*
       3430-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                   CIMM-RETRIEVE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-CLOSE.
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
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3460-LIMIT-STORAGE.
           IF CIMM-STORAGE-AMT < 0.00
               MOVE 0.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-AMT > 5000.00
               MOVE 5000.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3470-FLAG-STORAGE.
           IF CIMM-STORAGE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-OPEN.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-ITEM.
           IF CIMM-ITEM-AMT < 100.00
               MOVE 100.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-AMT > 10000.00
               MOVE 10000.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-STORAGE.
           IF CIMM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-AMT * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-OPEN.
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
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE ITEM AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3550-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CIMM-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED = CIMM-ITEM-AMT
                       * CIMM-MARGIN / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 12.00 TO CIMM-RETRIEVE-FEE-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 3.00 TO CIMM-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED = CIMM-ITEM-AMT
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-RETRIEVE-FEE-AMT > 0 AND CIMM-RETRIEVE-FEE-AMT < 5.00
               MOVE 5.00 TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           IF CIMM-RETRIEVE-FEE-AMT > 50.00
               MOVE 50.00 TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMM-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-EFFECTIVE.
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
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETRIEVE GRID TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RETRIEVE-GRID-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIMM-RETRIEVE-FEE-AMT NOT <
                   WS-RETRIEVE-GRID-LOW(WS-TX)
                   MOVE WS-RETRIEVE-GRID-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 10
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-CHANNEL.
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
      * SPLIT IMAGE FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE CIMM-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-MATURITY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO CIMM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-STORAGE-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE CIMM-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-ITEM.
           IF CIMM-ITEM-AMT < 0.00
               MOVE 0.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-AMT > 1000.00
               MOVE 1000.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE AMT BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CAPTURE-SITE-CD
                   ADD CIMM-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-RETRIEVE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE IMAGE FEE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3670-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-HOLD-FLAG = 'Y'
                   COMPUTE CIMT-STORAGE-AMT ROUNDED = CIMM-IMAGE-FEE-YTD
                       * CIMM-PENALTY-PCT / 200
               WHEN CIMM-ITEM-TYPE-CD = 'CW'
                   MOVE 3.00 TO CIMT-STORAGE-AMT
               WHEN CIMM-ITEM-TYPE-CD = 'FB'
                   MOVE 12.00 TO CIMT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMT-STORAGE-AMT ROUNDED = CIMM-IMAGE-FEE-YTD
                       * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMT-STORAGE-AMT > 0 AND CIMT-STORAGE-AMT < 2.00
               MOVE 2.00 TO CIMT-STORAGE-AMT
           END-IF
           IF CIMT-STORAGE-AMT > 150.00
               MOVE 150.00 TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3680-FLAG-ITEM.
           IF CIMM-ITEM-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMM-ITEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3690-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMT-IMAGE-FEE-AMT *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMT-IMAGE-FEE-AMT *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-LAST-STMT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-CHANNEL.
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
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-MATURITY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN CLOSE DATE IS PAST
      * FB -> HN WHEN CLOSE DATE IS PAST
      * HN -> KX WHEN CLOSE DATE IS PAST
      * KX -> CW WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIMM-CLOSE-DATE > 0 AND CIMM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE IMAGE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CIMM-IMAGE-FEE-YTD NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3760-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 10
               MOVE 'Y' TO CIMM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-ITEM.
           IF CIMM-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETRIEVE FEE AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-CHANNEL.
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
      * ASSESS THE RETRIEVE FEE FEE ON THE ITEM YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3800-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMM-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED = CIMM-ITEM-YTD
                       * CIMM-FEE-PCT / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 3.00 TO CIMM-RETRIEVE-FEE-AMT
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 15.00 TO CIMM-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED = CIMM-ITEM-YTD
                       * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-RETRIEVE-FEE-AMT > 0 AND CIMM-RETRIEVE-FEE-AMT <
               10.00
               MOVE 10.00 TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           IF CIMM-RETRIEVE-FEE-AMT > 75.00
               MOVE 75.00 TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMM-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CAPTURE SITE CD:
      * BB -> CA WHEN MATURITY DATE IS PAST
      * CA -> PC WHEN MATURITY DATE IS PAST
      * PC -> BB WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-CAPTURE-SITE.
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
      * ASSESS THE STORAGE FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3820-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMT-IMAGE-FEE-AMT
                       * CIMM-MARGIN / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 10.00 TO CIMM-STORAGE-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 10.00 TO CIMM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMT-IMAGE-FEE-AMT
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-STORAGE-AMT > 0 AND CIMM-STORAGE-AMT < 10.00
               MOVE 10.00 TO CIMM-STORAGE-AMT
           END-IF
           IF CIMM-STORAGE-AMT > 50.00
               MOVE 50.00 TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD < 25.00
               MOVE 25.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 50000.00
               MOVE 50000.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3850-FLAG-STORAGE.
           IF CIMT-STORAGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMT-STORAGE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMT-STORAGE-AMT - (WS-WORK-AMT * 3)
           COMPUTE CIMT-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       3880-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMT-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-RETRY.
           IF CIMM-RETRY-CNT < 9999999
               ADD 1 TO CIMM-RETRY-CNT
           END-IF
           IF CIMM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-YTD - (WS-WORK-AMT * 12)
           COMPUTE CIMM-IMAGE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       3910-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMM-IMAGE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E959 REF NO REQUIRED
      * E868 ITEM AMT NEGATIVE
      * E468 ITEM AMT OVER 250000
      * E420 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-REF-NO = SPACES
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-AMT < 0
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-AMT > 250000.00
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN CIMM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-AMT - (WS-WORK-AMT * 4)
           COMPUTE CIMT-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3950-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE YTD FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3960-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                       CIMM-IMAGE-FEE-AMT * CIMM-MARGIN / 200
               WHEN CIMM-CHANNEL-CD = 'FW'
                   MOVE 10.00 TO CIMM-IMAGE-FEE-YTD
               WHEN CIMM-CHANNEL-CD = 'KL'
                   MOVE 20.00 TO CIMM-IMAGE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED =
                       CIMM-IMAGE-FEE-AMT * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-YTD > 0 AND CIMM-IMAGE-FEE-YTD < 2.00
               MOVE 2.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 50.00
               MOVE 50.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 100.00
               MOVE 100.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 1000.00
               MOVE 1000.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-POST.
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4000-FLAG-STORAGE.
           IF CIMT-STORAGE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-STORAGE.
           IF CIMM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-YTD * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMM-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-STATUS-CD
                   ADD CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE STORAGE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4030-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIMT-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED = CIMT-STORAGE-AMT
                       * CIMM-FEE-PCT / 200
               WHEN CIMM-BRANCH-CD = 'DJ'
                   MOVE 12.00 TO CIMT-IMAGE-FEE-AMT
               WHEN CIMM-BRANCH-CD = 'FX'
                   MOVE 5.00 TO CIMT-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED = CIMT-STORAGE-AMT
                       * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMT-IMAGE-FEE-AMT > 0 AND CIMT-IMAGE-FEE-AMT < 5.00
               MOVE 5.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 50.00
               MOVE 50.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE RETRIEVE FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4040-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-AMT ROUNDED =
                       CIMM-RETRIEVE-FEE-AMT * CIMM-PENALTY-PCT / 200
               WHEN CIMM-CAPTURE-SITE-CD = 'BB'
                   MOVE 3.00 TO CIMM-STORAGE-AMT
               WHEN CIMM-CAPTURE-SITE-CD = 'CA'
                   MOVE 5.00 TO CIMM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-AMT ROUNDED =
                       CIMM-RETRIEVE-FEE-AMT * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-STORAGE-AMT > 0 AND CIMM-STORAGE-AMT < 10.00
               MOVE 10.00 TO CIMM-STORAGE-AMT
           END-IF
           IF CIMM-STORAGE-AMT > 25.00
               MOVE 25.00 TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       4050-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMM-ITEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-ITEM.
           IF CIMT-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-ITEM-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMT-RETRIEVE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4070-LIMIT-STORAGE.
           IF CIMT-STORAGE-AMT < 100.00
               MOVE 100.00 TO CIMT-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-STORAGE-AMT > 1000.00
               MOVE 1000.00 TO CIMT-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ST'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN EFFECTIVE DATE IS PAST
      * FB -> HN WHEN EFFECTIVE DATE IS PAST
      * HN -> KX WHEN EFFECTIVE DATE IS PAST
      * KX -> CW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   IF CIMM-EFFECTIVE-DATE > 0 AND CIMM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF CIMM-EFFECTIVE-DATE > 0 AND CIMM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HN' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HN'
                   IF CIMM-EFFECTIVE-DATE > 0 AND CIMM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KX'
                   IF CIMM-EFFECTIVE-DATE > 0 AND CIMM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CW' TO CIMM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT < 100.00
               MOVE 100.00 TO CIMM-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-AMT > 1000.00
               MOVE 1000.00 TO CIMM-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4120-FLAG-STORAGE.
           IF CIMT-STORAGE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       4140-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-YTD ROUNDED =
                   CIMM-STORAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMT-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-AMT - (WS-WORK-AMT * 4)
           COMPUTE CIMM-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRIDX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRIDX-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-ITEM-GRIDX-LOW(WS-TX)
                   MOVE WS-ITEM-GRIDX-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4190-COUNT-RETRY.
           IF CIMM-RETRY-CNT < 9999999
               ADD 1 TO CIMM-RETRY-CNT
           END-IF
           IF CIMM-RETRY-CNT NOT < 12
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4200-LIMIT-STORAGE.
           IF CIMM-STORAGE-YTD < 10.00
               MOVE 10.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-YTD > 1000.00
               MOVE 1000.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE STORAGE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4210-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CIMM-STORAGE-AMT *
                       CIMM-MARGIN / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CIMM-STORAGE-AMT *
                       CIMM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ST'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       4230-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-AMT ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-PRORATED.

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
                   MOVE CIVAL-STORAGE-AMT
                     TO CIMM-STORAGE-AMT
                   MOVE CIVAL-STORAGE-YTD
                     TO CIMM-STORAGE-YTD
                   MOVE CIVAL-ITEM-YTD
                     TO CIMM-ITEM-YTD
                   MOVE CIVAL-EXEMPT-FLAG
                     TO CIMM-EXEMPT-FLAG
                   MOVE CIVAL-OVERRIDE-FLAG
                     TO CIMM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CIVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE CIRISK-REVIEW-FLAG
                     TO CIMM-REVIEW-FLAG
                   MOVE CIRISK-HOLD-FLAG
                     TO CIMM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-CIRISK' TO ABND-PARAGRAPH
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
                   MOVE CIPRC-IMAGE-FEE-AMT
                     TO CIMM-IMAGE-FEE-AMT
                   MOVE CIPRC-RETRIEVE-FEE-YTD
                     TO CIMM-RETRIEVE-FEE-YTD
                   MOVE CIPRC-RETRIEVE-FEE-AMT
                     TO CIMM-RETRIEVE-FEE-AMT
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
           DISPLAY 'CIMEDIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CIMEDIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
