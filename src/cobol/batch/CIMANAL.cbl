      *================================================================*
      * PROGRAM  : CIMANAL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHECK IMAGE ARCHIVE - ANALYSIS. READS THE CIM       *
      *            TRANSACTIONS, UPDATES THE CIM MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CHECK IMAGE CAPTURE INDEX,      *
      *            RETRIEVAL AND IMAGE STATEMENT FEES                  *
      * INPUT    : CIMMST (KSDS), CIMTRN, BANKCTL                      *
      * OUTPUT   : CIMOUT, CIMANRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CIVAL, CIPRC            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-06-19 CRP  ORIGINAL                                       *
      * 1991-11-13 TWH  IMAGE-FEE NOTICE TEXT PER LEGAL PRB9621        *
      * 1996-12-17 TWH  STORAGE FEE SCHEDULE UPDATE REQ7971            *
      * 1996-04-07 GAF  IMAGE-FEE ROUNDING - HALF UP CHG2160           *
      * 1997-04-07 CRP  ITEM RERUN PROTECTION CHG6030                  *
      * 2001-06-24 SKN  ITEM CUTOFF MOVED INC7319                      *
      * 2002-10-14 JLM  STORAGE STATUS CODES ADDED INC8917             *
      * 2002-01-19 PQR  RETRIEVE-FEE FEE SCHEDULE UPDATE CHG8088       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIMANAL.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "CIMANRPT"
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
                                           VALUE 18.000000.
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
      * WS-IMAGE-FE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-IMAGE-FE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000077000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000092000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000127500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000603000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000702000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025550000000863500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026550000000544500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027050000000805500'.
       01  WS-IMAGE-FE-BAND-TBL REDEFINES WS-IMAGE-FE-BAND-VALUES.
           05  WS-IMAGE-FE-BAND-ROW OCCURS 8 TIMES.
               10  WS-IMAGE-FE-BAND-LOW    PIC 9(09)V99.
               10  WS-IMAGE-FE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000769500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000509000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000807000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000073500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030050000000284500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030100000000492500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040100000000265000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040150000000772500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040400000000442000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00042900000000113000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00045400000000659500'.
       01  WS-STORAGE-SCHED-TBL REDEFINES WS-STORAGE-SCHED-VALUES.
           05  WS-STORAGE-SCHED-ROW OCCURS 11 TIMES.
               10  WS-STORAGE-SCHED-LOW    PIC 9(09)V99.
               10  WS-STORAGE-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-IMAGE-FE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-IMAGE-FE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000587000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000588500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000881500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000070500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004100000000548000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004200000000398000'.
       01  WS-IMAGE-FE-GRID-TBL REDEFINES WS-IMAGE-FE-GRID-VALUES.
           05  WS-IMAGE-FE-GRID-ROW OCCURS 6 TIMES.
               10  WS-IMAGE-FE-GRID-LOW    PIC 9(09)V99.
               10  WS-IMAGE-FE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RETRIEVE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETRIEVE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000502000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000249000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000888500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000209500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006150000000366500'.
       01  WS-RETRIEVE-SCHED-TBL REDEFINES WS-RETRIEVE-SCHED-VALUES.
           05  WS-RETRIEVE-SCHED-ROW OCCURS 5 TIMES.
               10  WS-RETRIEVE-SCHED-LOW   PIC 9(09)V99.
               10  WS-RETRIEVE-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CIMANAL  CHECK IMAGE ARCHIVE'.
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
               PERFORM 3000-SPLIT-ITEM
               PERFORM 3010-COUNT-EXCEPT
               PERFORM 3020-FLAG-STORAGE
               PERFORM 3030-LIMIT-RETRIEVE-FEE
               PERFORM 3040-SELECT-MARGIN
               PERFORM 3050-COUNT-NOTICE
               PERFORM 3060-AGE-LAST-STMT
               PERFORM 3070-PRORATE-RETRIEVE-FEE
               PERFORM 3080-EDIT-REJECT
               PERFORM 3090-SPLIT-IMAGE-FEE
               PERFORM 3100-SPLIT-IMAGE-FEE
               PERFORM 3110-PRORATE-STORAGE
               PERFORM 3120-SPLIT-RETRIEVE-FEE
               PERFORM 3130-DESCRIBE-ITEM-TYPE
               PERFORM 3140-ACCUM-CHANNEL
               PERFORM 3150-ACCRUE-RETRIEVE-FEE
               PERFORM 3160-SPLIT-STORAGE
               PERFORM 3170-COUNT-EXCEPT
               PERFORM 3180-ACCUM-STATUS
               PERFORM 3190-ASSESS-REVIEW
               PERFORM 3200-ACCRUE-ITEM
               PERFORM 3210-LIMIT-IMAGE-FEE
               PERFORM 3220-FLAG-ITEM
               PERFORM 3230-ACCRUE-ITEM
               PERFORM 3240-PRORATE-ITEM
               PERFORM 3250-ACCUM-CAPTURE-SITE
               PERFORM 3260-EDIT-OPERATOR
               PERFORM 3270-CHECK-NOTICE
               PERFORM 3280-SELECT-PENALTY-PCT
               PERFORM 3290-DESCRIBE-STATUS
               PERFORM 3300-ACCRUE-ITEM
               PERFORM 3310-FLAG-STORAGE
               PERFORM 3320-AGE-CLOSE
               PERFORM 3330-SET-BRANCH
               PERFORM 3340-FLAG-ITEM
               PERFORM 3350-EDIT-MEMO
               PERFORM 3360-ACCRUE-PENALTY-PCT
               PERFORM 3370-LIMIT-STORAGE
               PERFORM 3380-LIMIT-ITEM
               PERFORM 3390-DESCRIBE-CHANNEL
               PERFORM 3400-FLAG-ITEM
               PERFORM 3410-COUNT-NOTICE
               PERFORM 3420-PRORATE-ITEM
               PERFORM 3430-FLAG-STORAGE
               PERFORM 3440-DESCRIBE-TYPE
               PERFORM 3450-ASSESS-OVERRIDE
               PERFORM 3460-SET-CHANNEL
               PERFORM 3470-ACCUM-CAPTURE-SITE
               PERFORM 3480-SPLIT-RETRIEVE-FEE
               PERFORM 3490-PRORATE-ITEM
               PERFORM 3500-EDIT-CUST-ID
               PERFORM 3510-AGE-LAST-MAINT
               PERFORM 3520-AGE-LAST-STMT
               PERFORM 3530-ACCUM-CHANNEL
               PERFORM 3540-DESCRIBE-ITEM-TYPE
               PERFORM 3550-PRORATE-RETRIEVE-FEE
               PERFORM 3560-COUNT-SEQ
               PERFORM 3570-ASSESS-EXEMPT
               PERFORM 3580-AGE-LAST-STMT
               PERFORM 3590-ACCRUE-STORAGE
               PERFORM 3600-ASSESS-SUSPEND
               PERFORM 3610-FLAG-IMAGE-FEE
               PERFORM 3620-LIMIT-STORAGE
               PERFORM 3630-LIMIT-ITEM
               PERFORM 3640-FLAG-ITEM
               PERFORM 3650-ASSESS-REVERSAL
               PERFORM 3660-SPLIT-STORAGE
               PERFORM 3670-FLAG-RETRIEVE-FEE
               PERFORM 3680-SET-ITEM-TYPE
               PERFORM 3690-LIMIT-IMAGE-FEE
               PERFORM 3700-EDIT-ADDR
               PERFORM 3710-PRORATE-ITEM
               PERFORM 3720-ASSESS-REVIEW
               PERFORM 3730-CHECK-EFF
               PERFORM 3740-SET-STATUS
               PERFORM 3750-SELECT-PENALTY-PCT
               PERFORM 3760-ACCUM-STATUS
               PERFORM 3770-LIMIT-ITEM
               PERFORM 3780-ACCRUE-IMAGE-FEE
               PERFORM 3790-LIMIT-ITEM
               PERFORM 3800-ACCRUE-FEE-PCT
               PERFORM 3810-AGE-POST
               PERFORM 3820-DESCRIBE-TRAN
               PERFORM 3830-ASSESS-EXEMPT
               PERFORM 3840-SELECT-PENALTY-PCT
               PERFORM 3850-SELECT-FEE-PCT
               PERFORM 3860-DESCRIBE-BRANCH
               PERFORM 3870-SPLIT-IMAGE-FEE
               PERFORM 3880-ACCUM-CHANNEL
               PERFORM 3890-SELECT-FEE-PCT
               PERFORM 3900-ACCRUE-STORAGE
               PERFORM 3910-ACCUM-CHANNEL
               PERFORM 3920-ACCUM-BRANCH
               PERFORM 3930-FLAG-STORAGE
               PERFORM 3940-FLAG-STORAGE
               PERFORM 3950-ACCRUE-IMAGE-FEE
               PERFORM 3960-SELECT-MARGIN
               PERFORM 3970-DESCRIBE-CAPTURE-SITE
               PERFORM 3980-PRORATE-RETRIEVE-FEE
               PERFORM 3990-PRORATE-ITEM
               PERFORM 4000-ACCRUE-ITEM
               PERFORM 4010-SET-CHANNEL
               PERFORM 4020-SELECT-PENALTY-PCT
               PERFORM 4030-EDIT-REJECT
               PERFORM 4040-ACCRUE-RETRIEVE-FEE
               PERFORM 4050-CHECK-LAST-MAINT
               PERFORM 4060-FLAG-ITEM
               PERFORM 4070-DESCRIBE-ITEM-TYPE
               PERFORM 4080-ACCUM-CHANNEL
               PERFORM 4090-DESCRIBE-ITEM-TYPE
               PERFORM 4100-LIMIT-IMAGE-FEE
               PERFORM 4110-LIMIT-ITEM
               PERFORM 4120-COUNT-NOTICE
               PERFORM 4130-FLAG-STORAGE
               PERFORM 4140-COUNT-RETRY
               PERFORM 4150-CHECK-NEXT-DUE
               PERFORM 4160-DESCRIBE-TRAN
               PERFORM 4170-AGE-NEXT-DUE
               PERFORM 4180-AGE-LAST-STMT
               PERFORM 4190-ACCRUE-STORAGE
               PERFORM 4200-PRORATE-IMAGE-FEE
               PERFORM 4210-COUNT-PAYMENT
               PERFORM 4220-FLAG-IMAGE-FEE
               PERFORM 4230-FLAG-ITEM
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
      * SPLIT ITEM AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = CIMT-ITEM-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CIMT-ITEM-AMT - (WS-WORK-AMT * 2)
           COMPUTE CIMM-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3020-FLAG-STORAGE.
           IF CIMM-STORAGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3030-LIMIT-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT < 50.00
               MOVE 50.00 TO CIMT-RETRIEVE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-RETRIEVE-FEE-AMT > 10000.00
               MOVE 10000.00 TO CIMT-RETRIEVE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE BAND TIER FOR THE ITEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-BAND-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CIMM-ITEM-AMT NOT < WS-IMAGE-FE-BAND-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-BAND-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CIMM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-LAST-STMT.
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
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-RETRIEVE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ITEM YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       3070-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMM-ITEM-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E622 REJECT CD REQUIRED
      * E906 STORAGE YTD NEGATIVE
      * E653 STORAGE YTD OVER 50000
      * E726 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-REJECT-CD = SPACES
                   MOVE 'E622' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-YTD < 0
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-YTD > 50000.00
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN CIMT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE CIMM-RETRIEVE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT IMAGE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3100-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = CIMM-IMAGE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CIMM-IMAGE-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE CIMM-ITEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-STORAGE-AMT ROUNDED =
                   CIMM-ITEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT RETRIEVE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = CIMT-RETRIEVE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CIMT-RETRIEVE-FEE-AMT - (WS-WORK-AMT
               * 3)
           COMPUTE CIMM-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-CHANNEL.
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
      * DAILY INTEREST ON RETRIEVE FEE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-YTD * CIMM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIMM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE CIMM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-EXCEPT.
           IF CIMM-EXCEPT-CNT < 9999999
               ADD 1 TO CIMM-EXCEPT-CNT
           END-IF
           IF CIMM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ITEM AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-STATUS-CD
                   ADD CIMT-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE YTD FEE ON THE ITEM YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3190-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = CIMM-ITEM-YTD *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO CIMM-IMAGE-FEE-YTD
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO CIMM-IMAGE-FEE-YTD
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-YTD ROUNDED = CIMM-ITEM-YTD *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-YTD > 0 AND CIMM-IMAGE-FEE-YTD < 2.00
               MOVE 2.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 150.00
               MOVE 150.00 TO CIMM-IMAGE-FEE-YTD
           END-IF
           ADD CIMM-IMAGE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-ITEM.
           IF CIMM-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-AMT * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3210-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 50.00
               MOVE 50.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 500.00
               MOVE 500.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3220-FLAG-ITEM.
           IF CIMM-ITEM-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CIMM-ITEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-ITEM.
           IF CIMM-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-AMT * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CAPTURE-SITE-CD
                   ADD CIMT-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E006 OPERATOR REQUIRED
      * E547 STORAGE AMT NEGATIVE
      * E870 STORAGE AMT OVER 5000
      * E636 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-OPERATOR = SPACES
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT < 0
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT > 5000.00
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN CIMM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-NOTICE.
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
      * SELECT THE STORAGE SCHED TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CIMM-ITEM-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-STATUS.
           EVALUATE CIMM-STATUS-CD
               WHEN 'AJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-ITEM.
           IF CIMT-ITEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-ITEM-AMT * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMM-ITEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-STORAGE.
           IF CIMM-STORAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DJ -> FX WHEN NEXT DUE DATE IS PAST
      * FX -> MT WHEN NEXT DUE DATE IS PAST
      * MT -> UA WHEN NEXT DUE DATE IS PAST
      * UA -> VL WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FX'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF CIMM-NEXT-DUE-DATE > 0 AND CIMM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO CIMM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E984 MEMO REQUIRED
      * E972 IMAGE FEE YTD NEGATIVE
      * E678 IMAGE FEE YTD OVER 1000000
      * E461 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-MEMO = SPACES
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 1000000.00
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN CIMT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-STORAGE.
           IF CIMM-STORAGE-AMT < 10.00
               MOVE 10.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-STORAGE-AMT > 50000.00
               MOVE 50000.00 TO CIMM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-CHANNEL.
           EVALUATE CIMM-CHANNEL-CD
               WHEN 'FW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3400-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ITEM YTD.
      *----------------------------------------------------------------*
       3420-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-YTD ROUNDED =
                   CIMM-ITEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3430-FLAG-STORAGE.
           IF CIMT-STORAGE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-TYPE.
           EVALUATE CIMM-TYPE-CD
               WHEN 'CT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ITEM YTD FEE ON THE RETRIEVE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3450-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CIMM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CIMM-ITEM-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   COMPUTE CIMM-ITEM-YTD ROUNDED = CIMM-RETRIEVE-FEE-AMT
                       * CIMM-PENALTY-PCT / 200
               WHEN CIMM-ITEM-TYPE-CD = 'CW'
                   MOVE 10.00 TO CIMM-ITEM-YTD
               WHEN CIMM-ITEM-TYPE-CD = 'FB'
                   MOVE 20.00 TO CIMM-ITEM-YTD
               WHEN OTHER
                   COMPUTE CIMM-ITEM-YTD ROUNDED = CIMM-RETRIEVE-FEE-AMT
                       * CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF CIMM-ITEM-YTD > 0 AND CIMM-ITEM-YTD < 10.00
               MOVE 10.00 TO CIMM-ITEM-YTD
           END-IF
           IF CIMM-ITEM-YTD > 250.00
               MOVE 250.00 TO CIMM-ITEM-YTD
           END-IF
           ADD CIMM-ITEM-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-CHANNEL.
           EVALUATE CIMT-CHANNEL-CD
               WHEN 'BRN'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CIMT-EFF-DATE > 0 AND CIMT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CIMT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY CAPTURE SITE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-CAPTURE-SITE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CAPTURE-SITE-CD
                   ADD CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CAPTURE-SITE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-RETRIEVE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE CIMT-RETRIEVE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-ITEM-AMT ROUNDED =
                   CIMT-ITEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-ITEM-AMT
           END-IF
           ADD CIMM-ITEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E015 CUST ID REQUIRED
      * E029 IMAGE FEE YTD NEGATIVE
      * E650 IMAGE FEE YTD OVER 1000000
      * E011 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-CUST-ID = SPACES
                   MOVE 'E015' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD < 0
                   MOVE 'E029' TO WS-REJECT-CODE
               WHEN CIMM-IMAGE-FEE-YTD > 1000000.00
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN CIMT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-LAST-MAINT.
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
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-LAST-STMT.
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
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-ITEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-CHANNEL-CD
                   ADD CIMM-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-IMAGE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE IMAGE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETRIEVE FEE AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                   CIMM-IMAGE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-SEQ.
           IF CIMT-SEQ < 9999999
               ADD 1 TO CIMT-SEQ
           END-IF
           IF CIMT-SEQ NOT < 6
               MOVE 'Y' TO CIMM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ITEM AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3570-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMT-ITEM-AMT *
                       CIMM-PENALTY-PCT / 200
               WHEN CIMM-ITEM-TYPE-CD = 'CW'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN CIMM-ITEM-TYPE-CD = 'FB'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CIMT-ITEM-AMT *
                       CIMM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-LAST-STMT.
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
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-STORAGE.
           IF CIMM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-YTD * CIMM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETRIEVE FEE FEE ON THE IMAGE FEE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3600-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO CIMT-RETRIEVE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                       CIMT-IMAGE-FEE-AMT * CIMM-MARGIN / 200
               WHEN CIMM-TYPE-CD = 'CT'
                   MOVE 20.00 TO CIMT-RETRIEVE-FEE-AMT
               WHEN CIMM-TYPE-CD = 'NS'
                   MOVE 10.00 TO CIMT-RETRIEVE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMT-RETRIEVE-FEE-AMT ROUNDED =
                       CIMT-IMAGE-FEE-AMT * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMT-RETRIEVE-FEE-AMT > 0 AND CIMT-RETRIEVE-FEE-AMT < 1.00
               MOVE 1.00 TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           IF CIMT-RETRIEVE-FEE-AMT > 35.00
               MOVE 35.00 TO CIMT-RETRIEVE-FEE-AMT
           END-IF
           ADD CIMT-RETRIEVE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3610-FLAG-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMM-IMAGE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3620-LIMIT-STORAGE.
           IF CIMT-STORAGE-AMT < 50.00
               MOVE 50.00 TO CIMT-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-STORAGE-AMT > 500.00
               MOVE 500.00 TO CIMT-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3630-LIMIT-ITEM.
           IF CIMM-ITEM-YTD < 100.00
               MOVE 100.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-YTD > 500.00
               MOVE 500.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3640-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE ITEM AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3650-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CIMT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CIMM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-SUSPEND-FLAG = 'Y'
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMM-ITEM-AMT *
                       CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 20.00 TO CIMM-STORAGE-AMT
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 7.00 TO CIMM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMM-STORAGE-AMT ROUNDED = CIMM-ITEM-AMT *
                       CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMM-STORAGE-AMT > 0 AND CIMM-STORAGE-AMT < 2.00
               MOVE 2.00 TO CIMM-STORAGE-AMT
           END-IF
           IF CIMM-STORAGE-AMT > 35.00
               MOVE 35.00 TO CIMM-STORAGE-AMT
           END-IF
           ADD CIMM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = CIMM-STORAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CIMM-STORAGE-YTD - (WS-WORK-AMT * 2)
           COMPUTE CIMM-IMAGE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRIEVE FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3670-FLAG-RETRIEVE-FEE.
           IF CIMT-RETRIEVE-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-RETRIEVE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * CW -> FB WHEN OPEN DATE IS PAST
      * FB -> HN WHEN OPEN DATE IS PAST
      * HN -> KX WHEN OPEN DATE IS PAST
      * KX -> CW WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-ITEM-TYPE.
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
      * IMAGE FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3690-LIMIT-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT < 25.00
               MOVE 25.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-IMAGE-FEE-AMT > 500.00
               MOVE 500.00 TO CIMT-IMAGE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E074 ADDR REQUIRED
      * E999 STORAGE AMT NEGATIVE
      * E971 STORAGE AMT OVER 5000
      * E725 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMM-ADDR = SPACES
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT < 0
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-AMT > 5000.00
                   MOVE 'E971' TO WS-REJECT-CODE
               WHEN CIMM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETRIEVE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ITEM AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMT-ITEM-AMT ROUNDED =
                   CIMT-RETRIEVE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CIMT-ITEM-AMT
           END-IF
           ADD CIMT-ITEM-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE IMAGE FEE FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3720-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN CIMM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO CIMM-IMAGE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED = CIMM-STORAGE-AMT
                       * CIMM-FEE-PCT / 200
               WHEN CIMT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO CIMM-IMAGE-FEE-AMT
               WHEN CIMT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO CIMM-IMAGE-FEE-AMT
               WHEN OTHER
                   COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED = CIMM-STORAGE-AMT
                       * CIMM-FEE-PCT / 100
           END-EVALUATE
           IF CIMM-IMAGE-FEE-AMT > 0 AND CIMM-IMAGE-FEE-AMT < 10.00
               MOVE 10.00 TO CIMM-IMAGE-FEE-AMT
           END-IF
           IF CIMM-IMAGE-FEE-AMT > 100.00
               MOVE 100.00 TO CIMM-IMAGE-FEE-AMT
           END-IF
           ADD CIMM-IMAGE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-EFF.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO CIMM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AJ -> JY WHEN NOTICE DATE IS PAST
      * JY -> RU WHEN NOTICE DATE IS PAST
      * RU -> UR WHEN NOTICE DATE IS PAST
      * UR -> YW WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-STATUS.
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
      * SELECT THE STORAGE SCHED TIER FOR THE RETRIEVE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CIMM-RETRIEVE-FEE-YTD NOT <
                   WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-STATUS.
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
      * ITEM AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3770-LIMIT-ITEM.
           IF CIMT-ITEM-AMT < 0.00
               MOVE 0.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMT-ITEM-AMT > 500.00
               MOVE 500.00 TO CIMT-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-YTD * CIMM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-ITEM.
           IF CIMM-ITEM-AMT < 100.00
               MOVE 100.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-AMT > 1000.00
               MOVE 1000.00 TO CIMM-ITEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CIMM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CIMT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-POST.
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
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMT-RETRIEVE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ST'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE RETRIEVE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3830-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CIMM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CIMT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CIMM-PREFERRED-FLAG = 'Y'
                   COMPUTE CIMT-STORAGE-AMT ROUNDED =
                       CIMM-RETRIEVE-FEE-YTD * CIMM-MARGIN / 200
               WHEN CIMM-STATUS-CD = 'AJ'
                   MOVE 7.00 TO CIMT-STORAGE-AMT
               WHEN CIMM-STATUS-CD = 'JY'
                   MOVE 7.00 TO CIMT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE CIMT-STORAGE-AMT ROUNDED =
                       CIMM-RETRIEVE-FEE-YTD * CIMM-MARGIN / 100
           END-EVALUATE
           IF CIMT-STORAGE-AMT > 0 AND CIMT-STORAGE-AMT < 10.00
               MOVE 10.00 TO CIMT-STORAGE-AMT
           END-IF
           IF CIMT-STORAGE-AMT > 100.00
               MOVE 100.00 TO CIMT-STORAGE-AMT
           END-IF
           ADD CIMT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE BAND TIER FOR THE IMAGE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CIMT-IMAGE-FEE-AMT NOT < WS-IMAGE-FE-BAND-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE ITEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CIMM-ITEM-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO CIMM-FEE-PCT
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
       3860-DESCRIBE-BRANCH.
           EVALUATE CIMM-BRANCH-CD
               WHEN 'DJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-IMAGE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE CIMM-IMAGE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMT-CHANNEL-CD
                   ADD CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMM-IMAGE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE RETRIEVE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO CIMM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CIMM-RETRIEVE-FEE-AMT NOT <
                   WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO CIMM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-STORAGE.
           IF CIMT-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMT-STORAGE-AMT * CIMM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO CIMT-RETRIEVE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-CHANNEL.
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
      * ACCUMULATE ITEM AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CIMM-BRANCH-CD
                   ADD CIMT-ITEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CIMM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CIMT-ITEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3930-FLAG-STORAGE.
           IF CIMT-STORAGE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3940-FLAG-STORAGE.
           IF CIMM-STORAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON IMAGE FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-IMAGE-FEE-AMT * CIMM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE IMAGE FE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-BAND-RATE(1) TO CIMM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-IMAGE-FE-BAND-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-BAND-RATE(WS-TX) TO CIMM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CAPTURE SITE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-CAPTURE-SITE.
           EVALUATE CIMM-CAPTURE-SITE-CD
               WHEN 'BB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETRIEVE FEE YTD.
      *----------------------------------------------------------------*
       3980-PRORATE-RETRIEVE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD ROUNDED =
                   CIMM-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-RETRIEVE-FEE-YTD
           END-IF
           ADD CIMM-RETRIEVE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE ITEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3990-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CIMM-ITEM-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ITEM YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-ITEM.
           IF CIMM-ITEM-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-ITEM-YTD * CIMM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CIMT-ITEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4010-SET-CHANNEL.
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
      * SELECT THE IMAGE FE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-IMAGE-FE-BAND-RATE(1) TO CIMM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-IMAGE-FE-BAND-LOW(WS-TX)
                   MOVE WS-IMAGE-FE-BAND-RATE(WS-TX) TO CIMM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CIMM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CIMM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E927 REJECT CD REQUIRED
      * E062 STORAGE YTD NEGATIVE
      * E215 STORAGE YTD OVER 25000
      * E957 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CIMT-REJECT-CD = SPACES
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-YTD < 0
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN CIMM-STORAGE-YTD > 25000.00
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN CIMM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETRIEVE FEE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-RETRIEVE-FEE.
           IF CIMM-RETRIEVE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-RETRIEVE-FEE-YTD * CIMM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIMM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4050-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF CIMM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CIMM-LAST-MAINT-DATE TO DTU-DATE-1
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
      * ITEM AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4060-FLAG-ITEM.
           IF CIMT-ITEM-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-ITEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE IMAGE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-CHANNEL.
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
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-ITEM-TYPE.
           EVALUATE CIMM-ITEM-TYPE-CD
               WHEN 'CW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * IMAGE FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-IMAGE-FEE.
           IF CIMM-IMAGE-FEE-YTD < 10.00
               MOVE 10.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-IMAGE-FEE-YTD > 10000.00
               MOVE 10000.00 TO CIMM-IMAGE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-ITEM.
           IF CIMM-ITEM-YTD < 0.00
               MOVE 0.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CIMM-ITEM-YTD > 5000.00
               MOVE 5000.00 TO CIMM-ITEM-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-NOTICE.
           IF CIMM-NOTICE-CNT < 9999999
               ADD 1 TO CIMM-NOTICE-CNT
           END-IF
           IF CIMM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO CIMM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4130-FLAG-STORAGE.
           IF CIMM-STORAGE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CIMM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-RETRY.
           IF CIMM-RETRY-CNT < 9999999
               ADD 1 TO CIMM-RETRY-CNT
           END-IF
           IF CIMM-RETRY-CNT NOT < 5
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO CIMT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4160-DESCRIBE-TRAN.
           EVALUATE CIMT-TRAN-CD
               WHEN 'CP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ST'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-NEXT-DUE.
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
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-LAST-STMT.
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
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CIMM-IMAGE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4190-ACCRUE-STORAGE.
           IF CIMM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CIMM-STORAGE-AMT * CIMM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO CIMT-IMAGE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO IMAGE FEE AMT.
      *----------------------------------------------------------------*
       4200-PRORATE-IMAGE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CIMM-IMAGE-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CIMM-IMAGE-FEE-AMT
           END-IF
           ADD CIMM-IMAGE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-PAYMENT.
           IF CIMM-PAYMENT-CNT < 9999999
               ADD 1 TO CIMM-PAYMENT-CNT
           END-IF
           IF CIMM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO CIMM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * IMAGE FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4220-FLAG-IMAGE-FEE.
           IF CIMT-IMAGE-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-IMAGE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4230-FLAG-ITEM.
           IF CIMT-ITEM-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CIMT-ITEM-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
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
                   MOVE CIVAL-STORAGE-YTD
                     TO CIMM-STORAGE-YTD
                   MOVE CIVAL-ITEM-YTD
                     TO CIMM-ITEM-YTD
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
                   MOVE CIPRC-RETRIEVE-FEE-YTD
                     TO CIMM-RETRIEVE-FEE-YTD
                   MOVE CIPRC-ITEM-YTD
                     TO CIMM-ITEM-YTD
                   MOVE CIPRC-STORAGE-AMT
                     TO CIMM-STORAGE-AMT
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
           DISPLAY 'CIMANAL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CIMANAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
