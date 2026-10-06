      *================================================================*
      * PROGRAM  : CIMRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHECK IMAGE ARCHIVE - RECONCILIATION. READS THE CIM *
      *            TRANSACTIONS, UPDATES THE CIM MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CHECK IMAGE CAPTURE INDEX,      *
      *            RETRIEVAL AND IMAGE STATEMENT FEES                  *
      * INPUT    : CIMMST (KSDS), CIMTRN, BANKCTL                      *
      * OUTPUT   : CIMOUT, CIMRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CIVAL, CIPRC, CIRISK    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-03-19 SKN  ORIGINAL                                       *
      * 1996-01-02 PQR  PERFORMANCE - STORAGE READ AHEAD CHG8826       *
      * 1996-01-11 EWS  IMAGE-FEE LIMIT REVISED REQ7263                *
      * 1996-02-11 BHS  STORAGE CUTOFF MOVED REQ1238                   *
      * 1997-12-22 CRP  RETRIEVE-FEE RERUN PROTECTION REQ6188          *
      * 2002-06-24 RJT  AUDIT FINDING - RETRIEVE-FEE CONTROL TOTALS INC*
      * 2003-03-02 MKL  IMAGE-FEE TABLE EXPANDED INC8120               *
      * 2004-08-11 CRP  RETRIEVE-FEE CUTOFF MOVED INC9968              *
      * 2004-12-17 LMB  RETRIEVE-FEE LIMIT REVISED INC1555             *
      * 2008-09-09 GAF  IMAGE-FEE STATUS CODES ADDED INC0672           *
      * 2009-03-01 LMB  STORAGE CUTOFF MOVED PRB9780                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIMRCON.
       AUTHOR. C R PATEL.

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
           SELECT REPORT-FILE ASSIGN TO "CIMRCRPT"
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
                                           VALUE 9.000000.
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
      * WS-ITEM-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ITEM-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000111000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000067000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000866000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002100000000048000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000662000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000147000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000806000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022700000000682000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022800000000696000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023300000000404500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023400000000127000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023450000000813500'.
       01  WS-ITEM-GRID-TBL REDEFINES WS-ITEM-GRID-VALUES.
           05  WS-ITEM-GRID-ROW OCCURS 12 TIMES.
               10  WS-ITEM-GRID-LOW        PIC 9(09)V99.
               10  WS-ITEM-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-IMAGE-FE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-IMAGE-FE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000649000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000832500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000870500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000662000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015200000000188000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016200000000712000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026200000000689500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026300000000459000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028800000000034000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038800000000018000'.
       01  WS-IMAGE-FE-TIER-TBL REDEFINES WS-IMAGE-FE-TIER-VALUES.
           05  WS-IMAGE-FE-TIER-ROW OCCURS 10 TIMES.
               10  WS-IMAGE-FE-TIER-LOW    PIC 9(09)V99.
               10  WS-IMAGE-FE-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CIMRCON  CHECK IMAGE ARCHIVE'.
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
       COPY CIPRCP.
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
               PERFORM 3000-PRORATE-IMAGE-FEE
               PERFORM 3010-LIMIT-ITEM
               PERFORM 3020-ASSESS-EXEMPT
               PERFORM 3030-ACCRUE-PENALTY-PCT
               PERFORM 3040-PRORATE-STORAGE
               PERFORM 3050-FLAG-RETRIEVE-FEE
               PERFORM 3060-SPLIT-ITEM
               PERFORM 3070-ACCRUE-ITEM
               PERFORM 3080-DESCRIBE-TYPE
               PERFORM 3090-SPLIT-ITEM
               PERFORM 3100-AGE-POST
               PERFORM 3110-AGE-EFFECTIVE
               PERFORM 3120-DESCRIBE-TYPE
               PERFORM 3130-ACCRUE-FEE-PCT
               PERFORM 3140-PRORATE-STORAGE
               PERFORM 3150-COUNT-RETRY
               PERFORM 3160-CHECK-EFF
               PERFORM 3170-ASSESS-REVERSAL
               PERFORM 3180-ACCRUE-IMAGE-FEE
               PERFORM 3190-ASSESS-HOLD
               PERFORM 3200-ASSESS-REVERSAL
               PERFORM 3210-SET-BRANCH
               PERFORM 3220-LIMIT-IMAGE-FEE
               PERFORM 3230-COUNT-SEQ
               PERFORM 3240-COUNT-EXCEPT
               PERFORM 3250-SELECT-FEE-PCT
               PERFORM 3260-SET-BRANCH
               PERFORM 3270-ACCRUE-IMAGE-FEE
               PERFORM 3280-PRORATE-RETRIEVE-FEE
               PERFORM 3290-ACCRUE-MARGIN
               PERFORM 3300-COUNT-NOTICE
               PERFORM 3310-CHECK-NOTICE
               PERFORM 3320-SET-CHANNEL
               PERFORM 3330-ACCUM-BRANCH
               PERFORM 3340-ACCRUE-FEE-PCT
               PERFORM 3350-ACCUM-CHANNEL
               PERFORM 3360-ACCUM-CAPTURE-SITE
               PERFORM 3370-SELECT-FEE-PCT
               PERFORM 3380-ASSESS-REVERSAL
               PERFORM 3390-EDIT-NAME
               PERFORM 3400-LIMIT-STORAGE
               PERFORM 3410-EDIT-CONTACT
               PERFORM 3420-SELECT-MARGIN
               PERFORM 3430-PRORATE-RETRIEVE-FEE
               PERFORM 3440-AGE-EFF
               PERFORM 3450-SELECT-PENALTY-PCT
               PERFORM 3460-ACCUM-TRAN
               PERFORM 3470-AGE-LAST-STMT
               PERFORM 3480-LIMIT-ITEM
               PERFORM 3490-ACCUM-BRANCH
               PERFORM 3500-LIMIT-ITEM
               PERFORM 3510-EDIT-NAME
               PERFORM 3520-COUNT-EXCEPT
               PERFORM 3530-AGE-EFF
               PERFORM 3540-SELECT-FEE-PCT
               PERFORM 3550-LIMIT-ITEM
               PERFORM 3560-DESCRIBE-TYPE
               PERFORM 3570-ASSESS-REVIEW
               PERFORM 3580-SET-ITEM-TYPE
               PERFORM 3590-DESCRIBE-ITEM-TYPE
               PERFORM 3600-ACCUM-CHANNEL
               PERFORM 3610-SET-TYPE
               PERFORM 3620-SELECT-PENALTY-PCT
               PERFORM 3630-LIMIT-ITEM
               PERFORM 3640-ASSESS-REVIEW
               PERFORM 3650-CHECK-POST
               PERFORM 3660-ACCRUE-RETRIEVE-FEE
               PERFORM 3670-SPLIT-ITEM
               PERFORM 3680-AGE-CLOSE
               PERFORM 3690-SET-TYPE
               PERFORM 3700-LIMIT-ITEM
               PERFORM 3710-EDIT-REJECT
               PERFORM 3720-SPLIT-STORAGE
               PERFORM 3730-COUNT-EXCEPT
               PERFORM 3740-SET-CHANNEL
               PERFORM 3750-EDIT-OPERATOR
               PERFORM 3760-SELECT-MARGIN
               PERFORM 3770-ASSESS-EXEMPT
               PERFORM 3780-LIMIT-IMAGE-FEE
               PERFORM 3790-SELECT-PENALTY-PCT
               PERFORM 3800-DESCRIBE-CHANNEL
               PERFORM 3810-EDIT-ADDR
               PERFORM 3820-LIMIT-ITEM
               PERFORM 3830-SELECT-PENALTY-PCT
               PERFORM 3840-ACCRUE-IMAGE-FEE
               PERFORM 3850-ACCRUE-STORAGE
               PERFORM 3860-LIMIT-STORAGE
               PERFORM 3870-SPLIT-IMAGE-FEE
               PERFORM 3880-ACCRUE-IMAGE-FEE
               PERFORM 3890-SELECT-FEE-PCT
               PERFORM 3900-CHECK-CLOSE
               PERFORM 3910-AGE-CLOSE
               PERFORM 3920-CHECK-LAST-STMT
               PERFORM 3930-FLAG-ITEM
               PERFORM 3940-EDIT-ADDR
               PERFORM 3950-LIMIT-RETRIEVE-FEE
               PERFORM 3960-ACCRUE-PENALTY-PCT
               PERFORM 3970-DESCRIBE-TYPE
               PERFORM 3980-CHECK-CLOSE
               PERFORM 3990-SET-TYPE
               PERFORM 4000-CHECK-EFFECTIVE
               PERFORM 4010-SPLIT-ITEM
               PERFORM 4020-SPLIT-ITEM
               PERFORM 4030-ACCUM-CHANNEL
               PERFORM 4040-EDIT-CONTACT
               PERFORM 7010-CALL-CIVAL
               PERFORM 7020-CALL-CIPRC
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
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                   CIMM-STORAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-ITEM.
           IF CIMT-ITEM-AMT < 10.00
               MOVE 10.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-ITEM-AMT > 5000.00
               MOVE 5000.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE ITEM AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3020-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CIMM-ITEM-AMT *
                       CIMM-FEE-PCT / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CIMM-ITEM-AMT *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3050-FLAG-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-AMT - (WS-WORK-AMT * 4)
           COMPUTE CIMM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-ITEM.
           IF CIMM-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMT-ITEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-YTD - (WS-WORK-AMT * 12)
           COMPUTE CIMM-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-POST.
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
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-IMAGE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMT-ITEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-STORAGE-AMT ROUNDED =
                   CIMM-ITEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-RETRY.
           IF CIMM-RETRY-CNT < 9999999
               ADD 1 TO CIMM-RETRY-CNT
           END-IF
           IF CIMM-RETRY-CNT NOT < 12
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-EFF.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CIMM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE YTD FEE ON THE RETRIEVE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3170-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-YTD ROUNDED =
                       CIMM-RETRIEVE-FEE-AMT * CIMM-MARGIN / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO CIMM-STORAGE-YTD
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO CIMM-STORAGE-YTD
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-YTD ROUNDED =
                       CIMM-RETRIEVE-FEE-AMT * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-STORAGE-YTD > 0 AND CIMM-STORAGE-YTD < 5.00
               MOVE 5.00 TO CIMM-STORAGE-YTD
           END-IF
           IF CIMM-STORAGE-YTD > 75.00
               MOVE 75.00 TO CIMM-STORAGE-YTD
           END-IF
           ADD CIMM-STORAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-AMT * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE STORAGE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3190-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CIMM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CIMM-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED =
                       CIMT-STORAGE-AMT * CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 12.00 TO CIMM-RETRIEVE-FEE-AMT
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 7.00 TO CIMM-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMM-RETRIEVE-FEE-AMT ROUNDED =
                       CIMT-STORAGE-AMT * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-RETRIEVE-FEE-AMT > 0 AND CIMM-RETRIEVE-FEE-AMT < 5.00
               MOVE 5.00 TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           IF CIMM-RETRIEVE-FEE-AMT > 50.00
               MOVE 50.00 TO CIMM-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMM-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE IMAGE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3200-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMT-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                       CIMM-IMAGE-FEE-YTD * CIMM-FEE-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO CIMT-IMAGE-FEE-AMT
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO CIMT-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-IMAGE-FEE-AMT ROUNDED =
                       CIMM-IMAGE-FEE-YTD * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMT-IMAGE-FEE-AMT > 0 AND CIMT-IMAGE-FEE-AMT < 2.00
               MOVE 2.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 35.00
               MOVE 35.00 TO CIMT-IMAGE-FEE-AMT
           END-IF
           ADD CIMT-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN MATURITY DATE IS PAST
      * FX -> MT WHEN MATURITY DATE IS PAST
      * MT -> UA WHEN MATURITY DATE IS PAST
      * UA -> VL WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-BRANCH.
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
      * IMAGE FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 100.00
               MOVE 100.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 1000.00
               MOVE 1000.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 12
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO CIMM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CIMM-STORAGE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN MATURITY DATE IS PAST
      * FX -> MT WHEN MATURITY DATE IS PAST
      * MT -> UA WHEN MATURITY DATE IS PAST
      * UA -> VL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-BRANCH.
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
      * DAILY INTEREST ON IMAGE FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMT-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CIMM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIMM-RETRIEVE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-NOTICE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FW -> KL WHEN MATURITY DATE IS PAST
      * KL -> NB WHEN MATURITY DATE IS PAST
      * NB -> TP WHEN MATURITY DATE IS PAST
      * TP -> ZZ WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KL' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KL'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NB' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NB'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TP'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZZ' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-BRANCH.
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
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CIMM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-IMAGE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CHANNEL-CD
                   ADD CIMM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-CAPTURE-SITE.
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
      * SELECT THE IMAGE FE TIER TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-TIER-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIMM-STORAGE-AMT NOT < WS-IMAGE-FE-TIER-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-TIER-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RETRIEVE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3380-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMM-RETRIEVE-FEE-YTD
                       * CIMM-MARGIN / 200
               WHEN CIMM-CAPTURE-SITE-CD = 'BB'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN CIMM-CAPTURE-SITE-CD = 'CA'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMM-RETRIEVE-FEE-YTD
                       * CIMM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E293 NAME REQUIRED
      * E540 IMAGE FEE AMT NEGATIVE
      * E350 IMAGE FEE AMT OVER 1000000
      * E619 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-NAME = SPACES
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-AMT < 0
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-AMT > 1000000.00
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN CIMM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3400-LIMIT-STORAGE.
           IF CIMM-STORAGE-YTD < 100.00
               MOVE 100.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-YTD > 5000.00
               MOVE 5000.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E867 CONTACT REQUIRED
      * E469 ITEM AMT NEGATIVE
      * E803 ITEM AMT OVER 1000000
      * E801 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CONTACT = SPACES
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-AMT < 0
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN CIMM-ITEM-AMT > 1000000.00
                   MOVE 'E803' TO WS-REJECT-CODE
               WHEN CIMM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE TIER TIER FOR THE RETRIEVE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-TIER-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIMM-RETRIEVE-FEE-YTD NOT <
                   WS-IMAGE-FE-TIER-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-TIER-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       3430-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-EFF.
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
      * SELECT THE ITEM GRID TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CIMM-ITEM-YTD NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-PENALTY-PCT
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
       3460-ACCUM-TRAN.
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
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-LAST-STMT.
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
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3480-LIMIT-ITEM.
           IF CIMT-ITEM-AMT < 10.00
               MOVE 10.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-ITEM-AMT > 2500.00
               MOVE 2500.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-BRANCH-CD
                   ADD CIMM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E302 NAME REQUIRED
      * E402 IMAGE FEE AMT NEGATIVE
      * E648 IMAGE FEE AMT OVER 25000
      * E620 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-NAME = SPACES
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-AMT < 0
                   MOVE 'E402' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-AMT > 25000.00
                   MOVE 'E648' TO WS-REJECT-CODE
               WHEN CIMT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO CIMM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-EFF.
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
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CIMT-STORAGE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-ITEM.
           IF CIMM-ITEM-YTD < 25.00
               MOVE 25.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-YTD > 5000.00
               MOVE 5000.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3560-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE IMAGE FEE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3570-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMM-IMAGE-FEE-YTD *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMM-ITEM-TYPE-CD = 'CW'
                   MOVE 12.00 TO CIMM-ITEM-AMT
               WHEN CIMM-ITEM-TYPE-CD = 'FB'
                   MOVE 7.00 TO CIMM-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMM-ITEM-AMT ROUNDED = CIMM-IMAGE-FEE-YTD *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-ITEM-AMT > 0 AND CIMM-ITEM-AMT < 5.00
               MOVE 5.00 TO CIMM-ITEM-AMT
           END-IF
           IF CIMM-ITEM-AMT > 50.00
               MOVE 50.00 TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN EFFECTIVE DATE IS PAST
      * FB -> HN WHEN EFFECTIVE DATE IS PAST
      * HN -> KX WHEN EFFECTIVE DATE IS PAST
      * KX -> CW WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-ITEM-TYPE.
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
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CHANNEL-CD
                   ADD CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CT -> NS WHEN LAST MAINT DATE IS PAST
      * NS -> TR WHEN LAST MAINT DATE IS PAST
      * TR -> CT WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   IF CIMM-LAST-MAINT-DATE > 0 AND CIMM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NS' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NS'
                   IF CIMM-LAST-MAINT-DATE > 0 AND CIMM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TR' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF CIMM-LAST-MAINT-DATE > 0 AND CIMM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CT' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3630-LIMIT-ITEM.
           IF CIMT-ITEM-AMT < 50.00
               MOVE 50.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-ITEM-AMT > 500.00
               MOVE 500.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3640-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   COMPUTE CIMT-STORAGE-AMT ROUNDED = CIMM-IMAGE-FEE-AMT
                       * CIMM-PENALTY-PCT / 200
               WHEN CIMT-TRAN-CD = 'CP'
                   MOVE 5.00 TO CIMT-STORAGE-AMT
               WHEN CIMT-TRAN-CD = 'RT'
                   MOVE 5.00 TO CIMT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMT-STORAGE-AMT ROUNDED = CIMM-IMAGE-FEE-AMT
                       * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMT-STORAGE-AMT > 0 AND CIMT-STORAGE-AMT < 10.00
               MOVE 10.00 TO CIMT-STORAGE-AMT
           END-IF
           IF CIMT-STORAGE-AMT > 250.00
               MOVE 250.00 TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-POST.
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
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-YTD * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMT-ITEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-CLOSE.
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
      * STATUS TRANSITIONS FOR TYPE CD:
      * CT -> NS WHEN NEXT REVIEW DATE IS PAST
      * NS -> TR WHEN NEXT REVIEW DATE IS PAST
      * TR -> CT WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   IF CIMM-NEXT-REVIEW-DATE > 0 AND
                       CIMM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NS' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NS'
                   IF CIMM-NEXT-REVIEW-DATE > 0 AND
                       CIMM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TR' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF CIMM-NEXT-REVIEW-DATE > 0 AND
                       CIMM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CT' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E544 REJECT CD REQUIRED
      * E992 IMAGE FEE YTD NEGATIVE
      * E931 IMAGE FEE YTD OVER 25000
      * E873 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-REJECT-CD = SPACES
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 25000.00
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN CIMM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E873' TO WS-REJECT-CODE
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
       3720-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMT-STORAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIMT-STORAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMT-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FW -> KL WHEN MATURITY DATE IS PAST
      * KL -> NB WHEN MATURITY DATE IS PAST
      * NB -> TP WHEN MATURITY DATE IS PAST
      * TP -> ZZ WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KL' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KL'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NB' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NB'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TP' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TP'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZZ' TO CIMM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E467 OPERATOR REQUIRED
      * E136 WORK AMT NEGATIVE
      * E770 WORK AMT OVER 250000
      * E621 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-OPERATOR = SPACES
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E770' TO WS-REJECT-CODE
               WHEN CIMM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE IMAGE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CIMT-IMAGE-FEE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3770-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIMT-ITEM-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMT-ITEM-AMT ROUNDED = WS-WORK-AMT-2 *
                       CIMM-FEE-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO CIMT-ITEM-AMT
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO CIMT-ITEM-AMT
               WHEN OTHER
                   COMPUTE CIMT-ITEM-AMT ROUNDED = WS-WORK-AMT-2 *
                       CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMT-ITEM-AMT > 0 AND CIMT-ITEM-AMT < 2.00
               MOVE 2.00 TO CIMT-ITEM-AMT
           END-IF
           IF CIMT-ITEM-AMT > 75.00
               MOVE 75.00 TO CIMT-ITEM-AMT
           END-IF
           ADD CIMT-ITEM-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD < 25.00
               MOVE 25.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 50000.00
               MOVE 50000.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE TIER TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-TIER-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CIMT-RETRIEVE-FEE-AMT NOT <
                   WS-IMAGE-FE-TIER-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-TIER-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E128 ADDR REQUIRED
      * E327 STORAGE AMT NEGATIVE
      * E675 STORAGE AMT OVER 1000000
      * E938 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E128' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT < 0
                   MOVE 'E327' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT > 1000000.00
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN CIMM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3820-LIMIT-ITEM.
           IF CIMM-ITEM-YTD < 0.00
               MOVE 0.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-YTD > 2500.00
               MOVE 2500.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CIMT-STORAGE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-STORAGE.
           IF CIMM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-RETRIEVE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-STORAGE.
           IF CIMM-STORAGE-YTD < 0.00
               MOVE 0.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-YTD > 2500.00
               MOVE 2500.00 TO CIMM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE CIMM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-ITEM-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ITEM GRID TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ITEM-GRID-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CIMM-RETRIEVE-FEE-AMT NOT < WS-ITEM-GRID-LOW(WS-TX)
                   MOVE WS-ITEM-GRID-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-CLOSE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CIMM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-CLOSE.
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
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-LAST-STMT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO CIMM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3930-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E672 ADDR REQUIRED
      * E698 ITEM AMT NEGATIVE
      * E092 ITEM AMT OVER 10000
      * E658 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT < 0
                   MOVE 'E698' TO WS-REJECT-CODE
               WHEN CIMT-ITEM-AMT > 10000.00
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN CIMM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-AMT < 10.00
               MOVE 10.00 TO CIMM-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-RETRIEVE-FEE-AMT > 5000.00
               MOVE 5000.00 TO CIMM-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-ITEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-CLOSE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CT -> NS WHEN MATURITY DATE IS PAST
      * NS -> TR WHEN MATURITY DATE IS PAST
      * TR -> CT WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NS' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NS'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF CIMM-MATURITY-DATE > 0 AND CIMM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CT' TO CIMM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMT-ITEM-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMT-ITEM-AMT - (WS-WORK-AMT * 3)
           COMPUTE CIMM-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ITEM YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMM-ITEM-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-ITEM-YTD - (WS-WORK-AMT * 4)
           COMPUTE CIMM-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-CHANNEL.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E538 CONTACT REQUIRED
      * E638 IMAGE FEE YTD NEGATIVE
      * E616 IMAGE FEE YTD OVER 250000
      * E926 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CONTACT = SPACES
                   MOVE 'E538' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E638' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 250000.00
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN CIMT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

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
                   MOVE CIVAL-STORAGE-AMT
                     TO CIMM-STORAGE-AMT
                   MOVE CIVAL-STORAGE-YTD
                     TO CIMM-STORAGE-YTD
                   MOVE CIVAL-ITEM-AMT
                     TO CIMM-ITEM-AMT
                   MOVE CIVAL-ITEM-YTD
                     TO CIMM-ITEM-YTD
                   MOVE CIVAL-EXEMPT-FLAG
                     TO CIMM-EXEMPT-FLAG
                   MOVE CIVAL-HOLD-FLAG
                     TO CIMM-HOLD-FLAG
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
                   MOVE CIPRC-STORAGE-AMT
                     TO CIMM-STORAGE-AMT
                   MOVE CIPRC-EXEMPT-FLAG
                     TO CIMM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CIPRC' TO ABND-PARAGRAPH
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
                   MOVE CIRISK-RETRIEVE-FEE-AMT
                     TO CIMM-RETRIEVE-FEE-AMT
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
           DISPLAY 'CIMRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CIMRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
