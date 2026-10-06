      *================================================================*
      * PROGRAM  : PBKPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PASSBOOK PRINTING - DAILY POSTING. READS THE PBK    *
      *            TRANSACTIONS, UPDATES THE PBK MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PASSBOOK SAVINGS UPDATE PRINTING*
      *            AND UNPOSTED ITEM CONTROL                           *
      * INPUT    : PBKMST (KSDS), PBKTRN, BANKCTL                      *
      * OUTPUT   : PBKOUT, PBKPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PBDTE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-09-23 JLM  ORIGINAL                                       *
      * 1999-09-26 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-02 DPS  PERFORMANCE - PRINTED READ AHEAD PRB6736       *
      * 2002-10-05 PQR  LINE TABLE EXPANDED PRB1353                    *
      * 2007-07-26 DPS  BALANCE LIMIT REVISED PRB8716                  *
      * 2008-10-25 RJT  LINE TABLE EXPANDED INC7923                    *
      * 2008-06-05 RJT  INTEREST STATUS CODES ADDED REQ3713            *
      * 2008-03-17 JLM  BALANCE RERUN PROTECTION PRB3525               *
      * 2013-06-04 EWS  LINE ROUNDING - HALF UP CHG0724                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBKPOST.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PBKMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PBKM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PBKTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PBKOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PBKPORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PBKMST.
       FD  TRAN-FILE.
       COPY PBKTRN.
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
                                           VALUE 8.000000.
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
      * WS-INTEREST-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000663000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000386500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000275500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000319500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005450000000808000'.
       01  WS-INTEREST-TIER-TBL REDEFINES WS-INTEREST-TIER-VALUES.
           05  WS-INTEREST-TIER-ROW OCCURS 5 TIMES.
               10  WS-INTEREST-TIER-LOW    PIC 9(09)V99.
               10  WS-INTEREST-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINTED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINTED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000381000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000737000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000455000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000573500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030050000000773000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031050000000692000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031550000000144500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031600000000695000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031650000000552000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032150000000033000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032650000000815500'.
       01  WS-PRINTED-GRID-TBL REDEFINES WS-PRINTED-GRID-VALUES.
           05  WS-PRINTED-GRID-ROW OCCURS 11 TIMES.
               10  WS-PRINTED-GRID-LOW     PIC 9(09)V99.
               10  WS-PRINTED-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-UNPOSTED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-UNPOSTED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000180000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000850000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000092000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000083000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000393000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000862000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018650000000286000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019150000000332000'.
       01  WS-UNPOSTED-GRID-TBL REDEFINES WS-UNPOSTED-GRID-VALUES.
           05  WS-UNPOSTED-GRID-ROW OCCURS 8 TIMES.
               10  WS-UNPOSTED-GRID-LOW    PIC 9(09)V99.
               10  WS-UNPOSTED-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PBKPOST  PASSBOOK PRINTING'.
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
       COPY PBDTEP.

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
               MOVE 'PBKMST' TO ABND-FILE-NAME
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
           MOVE PBKT-KEY TO PBKM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-PRINTER
               PERFORM 3010-LIMIT-ITEM
               PERFORM 3020-EDIT-MEMO
               PERFORM 3030-SELECT-FEE-PCT
               PERFORM 3040-SET-REGION
               PERFORM 3050-AGE-LAST-ACT
               PERFORM 3060-ASSESS-NOTICE-SENT
               PERFORM 3070-PRORATE-PRINTED
               PERFORM 3080-EDIT-CONTACT
               PERFORM 3090-ACCRUE-MARGIN
               PERFORM 3100-ACCRUE-LINE
               PERFORM 3110-ASSESS-EXEMPT
               PERFORM 3120-FLAG-PRINTED
               PERFORM 3130-ASSESS-EXEMPT
               PERFORM 3140-AGE-CLOSE
               PERFORM 3150-SELECT-FEE-PCT
               PERFORM 3160-ACCRUE-LINE
               PERFORM 3170-ACCRUE-INTEREST
               PERFORM 3180-LIMIT-INTEREST
               PERFORM 3190-CHECK-NEXT-REVIEW
               PERFORM 3200-EDIT-REJECT
               PERFORM 3210-COUNT-PAYMENT
               PERFORM 3220-ACCRUE-PRINTED
               PERFORM 3230-EDIT-CUST-ID
               PERFORM 3240-ACCUM-BRANCH
               PERFORM 3250-DESCRIBE-REGION
               PERFORM 3260-FLAG-CARRYFWD
               PERFORM 3270-COUNT-SEQ
               PERFORM 3280-CHECK-MATURITY
               PERFORM 3290-SET-PRINTER
               PERFORM 3300-EDIT-ADDR
               PERFORM 3310-COUNT-CYCLE
               PERFORM 3320-FLAG-ITEM
               PERFORM 3330-DESCRIBE-TRAN
               PERFORM 3340-SELECT-FEE-PCT
               PERFORM 3350-SPLIT-BALANCE
               PERFORM 3360-COUNT-PAYMENT
               PERFORM 3370-FLAG-CARRYFWD
               PERFORM 3380-SELECT-FEE-PCT
               PERFORM 3390-AGE-CLOSE
               PERFORM 3400-LIMIT-PRINTED
               PERFORM 3410-EDIT-NAME
               PERFORM 3420-CHECK-LAST-MAINT
               PERFORM 3430-ACCRUE-LINE
               PERFORM 3440-SELECT-FEE-PCT
               PERFORM 3450-SET-BOOK-TYPE
               PERFORM 3460-LIMIT-LINE
               PERFORM 3470-EDIT-ADDR
               PERFORM 3480-LIMIT-BALANCE
               PERFORM 3490-ACCRUE-CARRYFWD
               PERFORM 3500-SELECT-MARGIN
               PERFORM 3510-COUNT-ITEM
               PERFORM 3520-ACCUM-CHANNEL
               PERFORM 3530-SELECT-PENALTY-PCT
               PERFORM 3540-FLAG-ITEM
               PERFORM 3550-LIMIT-ITEM
               PERFORM 3560-FLAG-BALANCE
               PERFORM 3570-FLAG-BALANCE
               PERFORM 3580-LIMIT-ITEM
               PERFORM 3590-LIMIT-CARRYFWD
               PERFORM 3600-ACCRUE-UNPOSTED
               PERFORM 3610-ASSESS-AUTO-RENEW
               PERFORM 3620-ACCUM-CHANNEL
               PERFORM 3630-ACCUM-REGION
               PERFORM 3640-EDIT-NAME
               PERFORM 3650-COUNT-RETRY
               PERFORM 3660-COUNT-CYCLE
               PERFORM 3670-FLAG-PRINTED
               PERFORM 3680-ACCRUE-UNPOSTED
               PERFORM 3690-SELECT-MARGIN
               PERFORM 3700-AGE-POST
               PERFORM 3710-ACCRUE-BALANCE
               PERFORM 3720-LIMIT-BALANCE
               PERFORM 3730-AGE-NEXT-REVIEW
               PERFORM 3740-ACCRUE-PRINTED
               PERFORM 3750-ACCUM-TRAN
               PERFORM 3760-SET-BOOK-TYPE
               PERFORM 3770-SET-CHANNEL
               PERFORM 3780-AGE-NEXT-DUE
               PERFORM 3790-EDIT-OPERATOR
               PERFORM 3800-ACCUM-REGION
               PERFORM 3810-COUNT-SEQ
               PERFORM 3820-FLAG-ITEM
               PERFORM 3830-PRORATE-PRINTED
               PERFORM 3840-SPLIT-INTEREST
               PERFORM 3850-LIMIT-UNPOSTED
               PERFORM 3860-SPLIT-INTEREST
               PERFORM 3870-DESCRIBE-REGION
               PERFORM 3880-SELECT-PENALTY-PCT
               PERFORM 3890-EDIT-CUST-ID
               PERFORM 3900-SPLIT-CARRYFWD
               PERFORM 3910-ACCUM-BOOK-TYPE
               PERFORM 3920-FLAG-PRINTED
               PERFORM 3930-DESCRIBE-BRANCH
               PERFORM 3940-SPLIT-PRINTED
               PERFORM 3950-PRORATE-UNPOSTED
               PERFORM 3960-SELECT-MARGIN
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-COUNT-CYCLE
               PERFORM 3990-ACCUM-REGION
               PERFORM 4000-SPLIT-INTEREST
               PERFORM 4010-CHECK-EFFECTIVE
               PERFORM 4020-COUNT-PAYMENT
               PERFORM 4030-DESCRIBE-BOOK-TYPE
               PERFORM 4040-EDIT-MEMO
               PERFORM 4050-DESCRIBE-PRINTER
               PERFORM 4060-CHECK-LAST-ACT
               PERFORM 4070-LIMIT-PRINTED
               PERFORM 4080-DESCRIBE-BOOK-TYPE
               PERFORM 4090-CHECK-NEXT-REVIEW
               PERFORM 4100-ASSESS-NOTICE-SENT
               PERFORM 4110-LIMIT-CARRYFWD
               PERFORM 4120-PRORATE-INTEREST
               PERFORM 4130-EDIT-CONTACT
               PERFORM 4140-ASSESS-EXEMPT
               PERFORM 4150-ACCRUE-FEE-PCT
               PERFORM 4160-SELECT-PENALTY-PCT
               PERFORM 4170-ACCUM-REGION
               PERFORM 4180-EDIT-CUST-ID
               PERFORM 4190-SPLIT-CARRYFWD
               PERFORM 4200-SET-TRAN
               PERFORM 4210-DESCRIBE-CHANNEL
               PERFORM 4220-LIMIT-BALANCE
               PERFORM 4230-ACCUM-CHANNEL
               PERFORM 4240-FLAG-ITEM
               PERFORM 4250-DESCRIBE-CHANNEL
               PERFORM 4260-CHECK-CLOSE
               PERFORM 4270-AGE-POST
               PERFORM 4280-PRORATE-PRINTED
               PERFORM 4290-AGE-LAST-ACT
               PERFORM 4300-FLAG-PRINTED
               PERFORM 4310-SELECT-PENALTY-PCT
               PERFORM 4320-ACCRUE-INTEREST
               PERFORM 4330-ACCUM-CHANNEL
               PERFORM 4340-CHECK-EFFECTIVE
               PERFORM 4350-PRORATE-PRINTED
               PERFORM 4360-EDIT-REJECT
               PERFORM 4370-COUNT-CYCLE
               PERFORM 4380-ASSESS-HOLD
               PERFORM 4390-CHECK-EFF
               PERFORM 4400-SELECT-MARGIN
               PERFORM 4410-ASSESS-HOLD
               PERFORM 4420-AGE-EFF
               PERFORM 4430-LIMIT-CARRYFWD
               PERFORM 4440-ASSESS-REVIEW
               PERFORM 4450-SELECT-FEE-PCT
               PERFORM 4460-SELECT-FEE-PCT
               PERFORM 4470-LIMIT-CARRYFWD
               PERFORM 4480-PRORATE-INTEREST
               PERFORM 4490-PRORATE-INTEREST
               PERFORM 4500-PRORATE-CARRYFWD
               PERFORM 4510-SET-CHANNEL
               PERFORM 4520-FLAG-UNPOSTED
               PERFORM 4530-ACCUM-BRANCH
               PERFORM 4540-COUNT-RETRY
               PERFORM 4550-ACCRUE-INTEREST
               PERFORM 4560-COUNT-PAYMENT
               PERFORM 4570-ACCUM-SOURCE
               PERFORM 4580-DESCRIBE-REGION
               PERFORM 4590-SPLIT-CARRYFWD
               PERFORM 4600-SPLIT-CARRYFWD
               PERFORM 4610-SPLIT-BALANCE
               PERFORM 4620-COUNT-ITEM
               PERFORM 4630-SELECT-FEE-PCT
               PERFORM 4640-DESCRIBE-TRAN
               PERFORM 4650-AGE-POST
               PERFORM 4660-SET-CHANNEL
               PERFORM 4670-CHECK-EFF
               PERFORM 7010-CALL-PBDTE
               REWRITE PBK-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PBKMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PBK-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'EP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'EX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E949 MEMO REQUIRED
      * E621 LINE AMT NEGATIVE
      * E621 LINE AMT OVER 5000
      * E782 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT < 0
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT > 5000.00
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE CARRYFWD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBKM-CARRYFWD-YTD NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN POST DATE IS PAST
      * GL -> PE WHEN POST DATE IS PAST
      * PE -> RJ WHEN POST DATE IS PAST
      * RJ -> VB WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE UNPOSTED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3060-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-AMT ROUNDED = PBKM-UNPOSTED-YTD
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-BRANCH-CD = 'DF'
                   MOVE 15.00 TO PBKM-INTEREST-AMT
               WHEN PBKM-BRANCH-CD = 'NV'
                   MOVE 12.00 TO PBKM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-AMT ROUNDED = PBKM-UNPOSTED-YTD
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-AMT > 0 AND PBKM-INTEREST-AMT < 1.00
               MOVE 1.00 TO PBKM-INTEREST-AMT
           END-IF
           IF PBKM-INTEREST-AMT > 100.00
               MOVE 100.00 TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       3070-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-AMT ROUNDED =
                   PBKM-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E811 CONTACT REQUIRED
      * E010 CARRYFWD AMT NEGATIVE
      * E974 CARRYFWD AMT OVER 50000
      * E450 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CONTACT = SPACES
                   MOVE 'E811' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT < 0
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT > 50000.00
                   MOVE 'E974' TO WS-REJECT-CODE
               WHEN PBKT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBKT-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3100-ACCRUE-LINE.
           IF PBKM-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-LINE-AMT * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKT-CARRYFWD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD FEE ON THE BALANCE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3110-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-CARRYFWD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = PBKM-BALANCE-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-BOOK-TYPE-CD = 'CH'
                   MOVE 15.00 TO PBKM-CARRYFWD-AMT
               WHEN PBKM-BOOK-TYPE-CD = 'HG'
                   MOVE 15.00 TO PBKM-CARRYFWD-AMT
               WHEN OTHER
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = PBKM-BALANCE-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-CARRYFWD-AMT > 0 AND PBKM-CARRYFWD-AMT < 5.00
               MOVE 5.00 TO PBKM-CARRYFWD-AMT
           END-IF
           IF PBKM-CARRYFWD-AMT > 25.00
               MOVE 25.00 TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRINTED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3120-FLAG-PRINTED.
           IF PBKT-PRINTED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKT-PRINTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CARRYFWD YTD FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3130-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-CARRYFWD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-CARRYFWD-YTD ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-BRANCH-CD = 'DF'
                   MOVE 7.00 TO PBKM-CARRYFWD-YTD
               WHEN PBKM-BRANCH-CD = 'NV'
                   MOVE 20.00 TO PBKM-CARRYFWD-YTD
               WHEN OTHER
                   COMPUTE PBKM-CARRYFWD-YTD ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-CARRYFWD-YTD > 0 AND PBKM-CARRYFWD-YTD < 5.00
               MOVE 5.00 TO PBKM-CARRYFWD-YTD
           END-IF
           IF PBKM-CARRYFWD-YTD > 150.00
               MOVE 150.00 TO PBKM-CARRYFWD-YTD
           END-IF
           ADD PBKM-CARRYFWD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED GRID TIER FOR THE PRINTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-GRID-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKT-PRINTED-AMT NOT < WS-UNPOSTED-GRID-LOW(WS-TX)
                   MOVE WS-UNPOSTED-GRID-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-LINE.
           IF PBKM-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-LINE-AMT * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-INTEREST.
           IF PBKM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-YTD * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-INTEREST.
           IF PBKT-INTEREST-AMT < 0.00
               MOVE 0.00 TO PBKT-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-INTEREST-AMT > 50000.00
               MOVE 50000.00 TO PBKT-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E772 REJECT CD REQUIRED
      * E022 CARRYFWD AMT NEGATIVE
      * E208 CARRYFWD AMT OVER 5000
      * E964 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-REJECT-CD = SPACES
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT < 0
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT > 5000.00
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN PBKM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-PRINTED.
           IF PBKT-PRINTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-PRINTED-AMT * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E640 CUST ID REQUIRED
      * E298 INTEREST YTD NEGATIVE
      * E529 INTEREST YTD OVER 250000
      * E536 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-YTD < 0
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-YTD > 250000.00
                   MOVE 'E529' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3260-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3270-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 5
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3280-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN NEXT REVIEW DATE IS PAST
      * EP -> EX WHEN NEXT REVIEW DATE IS PAST
      * EX -> JE WHEN NEXT REVIEW DATE IS PAST
      * JE -> MR WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E401 ADDR REQUIRED
      * E122 WORK AMT NEGATIVE
      * E083 WORK AMT OVER 250000
      * E420 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-ADDR = SPACES
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN PBKM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3320-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED GRID TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-GRID-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKM-BALANCE-AMT NOT < WS-UNPOSTED-GRID-LOW(WS-TX)
                   MOVE WS-UNPOSTED-GRID-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-YTD - (WS-WORK-AMT * 4)
           COMPUTE PBKM-PRINTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3370-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED GRID TIER FOR THE PRINTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-GRID-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKM-PRINTED-AMT NOT < WS-UNPOSTED-GRID-LOW(WS-TX)
                   MOVE WS-UNPOSTED-GRID-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3400-LIMIT-PRINTED.
           IF PBKT-PRINTED-AMT < 25.00
               MOVE 25.00 TO PBKT-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-PRINTED-AMT > 1000.00
               MOVE 1000.00 TO PBKT-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E676 NAME REQUIRED
      * E127 LINE AMT NEGATIVE
      * E441 LINE AMT OVER 10000
      * E255 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN PBKT-LINE-AMT < 0
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN PBKT-LINE-AMT > 10000.00
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN PBKM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E255' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-LINE.
           IF PBKT-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-LINE-AMT * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-PRINTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOOK TYPE CD:
      * CH -> HG WHEN MATURITY DATE IS PAST
      * HG -> KB WHEN MATURITY DATE IS PAST
      * KB -> KW WHEN MATURITY DATE IS PAST
      * KW -> PA WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HG' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HG'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KB' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KB'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PA' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LINE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3460-LIMIT-LINE.
           IF PBKM-LINE-YTD < 25.00
               MOVE 25.00 TO PBKM-LINE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-LINE-YTD > 2500.00
               MOVE 2500.00 TO PBKM-LINE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E065 ADDR REQUIRED
      * E225 INTEREST AMT NEGATIVE
      * E771 INTEREST AMT OVER 25000
      * E881 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-ADDR = SPACES
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-AMT < 0
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-AMT > 25000.00
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-BALANCE.
           IF PBKM-BALANCE-YTD < 50.00
               MOVE 50.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-YTD > 5000.00
               MOVE 5000.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-CARRYFWD-YTD * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBKM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3500-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PBKM-BALANCE-YTD NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 6
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3520-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE PRINTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PBKT-PRINTED-AMT NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3550-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-BALANCE.
           IF PBKM-BALANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3570-FLAG-BALANCE.
           IF PBKM-BALANCE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3580-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-YTD < 100.00
               MOVE 100.00 TO PBKM-CARRYFWD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-YTD > 10000.00
               MOVE 10000.00 TO PBKM-CARRYFWD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-UNPOSTED-YTD * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINTED FEE ON THE UNPOSTED YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3610-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-PRINTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKM-UNPOSTED-YTD
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 5.00 TO PBKM-PRINTED-AMT
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 7.00 TO PBKM-PRINTED-AMT
               WHEN OTHER
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKM-UNPOSTED-YTD
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-PRINTED-AMT > 0 AND PBKM-PRINTED-AMT < 2.00
               MOVE 2.00 TO PBKM-PRINTED-AMT
           END-IF
           IF PBKM-PRINTED-AMT > 50.00
               MOVE 50.00 TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD PBKM-LINE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-REGION-CD
                   ADD PBKM-LINE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E001 NAME REQUIRED
      * E919 BALANCE YTD NEGATIVE
      * E219 BALANCE YTD OVER 50000
      * E401 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E001' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-YTD < 0
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-YTD > 50000.00
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN PBKT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 3
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3670-FLAG-PRINTED.
           IF PBKT-PRINTED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKT-PRINTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-UNPOSTED.
           IF PBKM-UNPOSTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-UNPOSTED-AMT * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE PRINTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PBKT-PRINTED-AMT NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-BALANCE.
           IF PBKM-BALANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-BALANCE-AMT * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3720-LIMIT-BALANCE.
           IF PBKM-BALANCE-YTD < 100.00
               MOVE 100.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-YTD > 500.00
               MOVE 500.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0 AND PBKM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-PRINTED.
           IF PBKM-PRINTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-PRINTED-YTD * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKT-CARRYFWD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-TRAN-CD
                   ADD PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOOK TYPE CD:
      * CH -> HG WHEN NEXT REVIEW DATE IS PAST
      * HG -> KB WHEN NEXT REVIEW DATE IS PAST
      * KB -> KW WHEN NEXT REVIEW DATE IS PAST
      * KW -> PA WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HG' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HG'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KB' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KB'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KW' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PA' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN OPEN DATE IS PAST
      * D -> H WHEN OPEN DATE IS PAST
      * H -> L WHEN OPEN DATE IS PAST
      * L -> M WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E535 OPERATOR REQUIRED
      * E772 WORK AMT 2 NEGATIVE
      * E496 WORK AMT 2 OVER 10000
      * E656 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-OPERATOR = SPACES
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E656' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-REGION-CD
                   ADD PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 12
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3820-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-AMT ROUNDED =
                   PBKM-BALANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKT-INTEREST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKT-INTEREST-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKT-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3850-LIMIT-UNPOSTED.
           IF PBKM-UNPOSTED-AMT < 50.00
               MOVE 50.00 TO PBKM-UNPOSTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-UNPOSTED-AMT > 500.00
               MOVE 500.00 TO PBKM-UNPOSTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3860-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKT-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKT-INTEREST-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-BALANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE UNPOSTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PBKM-UNPOSTED-YTD NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E657 CUST ID REQUIRED
      * E636 LINE YTD NEGATIVE
      * E689 LINE YTD OVER 5000
      * E839 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD < 0
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD > 5000.00
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE PBKM-CARRYFWD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY BOOK TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-BOOK-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BOOK-TYPE-CD
                   ADD PBKM-LINE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BOOK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3920-FLAG-PRINTED.
           IF PBKT-PRINTED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKT-PRINTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-UNPOSTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PBKT-LINE-AMT NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE CARRYFWD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBKT-CARRYFWD-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-REGION-CD
                   ADD PBKT-LINE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-LINE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKT-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKT-INTEREST-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E144 MEMO REQUIRED
      * E499 BALANCE AMT NEGATIVE
      * E650 BALANCE AMT OVER 50000
      * E204 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-AMT < 0
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-AMT > 50000.00
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4070-LIMIT-PRINTED.
           IF PBKM-PRINTED-AMT < 100.00
               MOVE 100.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-AMT > 2500.00
               MOVE 2500.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE FEE ON THE INTEREST AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4100-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBKT-LINE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKT-LINE-AMT ROUNDED = PBKT-INTEREST-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-BOOK-TYPE-CD = 'CH'
                   MOVE 20.00 TO PBKT-LINE-AMT
               WHEN PBKM-BOOK-TYPE-CD = 'HG'
                   MOVE 20.00 TO PBKT-LINE-AMT
               WHEN OTHER
                   COMPUTE PBKT-LINE-AMT ROUNDED = PBKT-INTEREST-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKT-LINE-AMT > 0 AND PBKT-LINE-AMT < 10.00
               MOVE 10.00 TO PBKT-LINE-AMT
           END-IF
           IF PBKT-LINE-AMT > 25.00
               MOVE 25.00 TO PBKT-LINE-AMT
           END-IF
           ADD PBKT-LINE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-CARRYFWD.
           IF PBKT-CARRYFWD-AMT < 25.00
               MOVE 25.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-CARRYFWD-AMT > 10000.00
               MOVE 10000.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       4120-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-INTEREST-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E914 CONTACT REQUIRED
      * E280 UNPOSTED AMT NEGATIVE
      * E443 UNPOSTED AMT OVER 10000
      * E353 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CONTACT = SPACES
                   MOVE 'E914' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-AMT < 0
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-AMT > 10000.00
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN PBKM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE BALANCE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4140-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-BALANCE-YTD ROUNDED = PBKM-BALANCE-AMT *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 10.00 TO PBKM-BALANCE-YTD
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 20.00 TO PBKM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE PBKM-BALANCE-YTD ROUNDED = PBKM-BALANCE-AMT *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-BALANCE-YTD > 0 AND PBKM-BALANCE-YTD < 2.00
               MOVE 2.00 TO PBKM-BALANCE-YTD
           END-IF
           IF PBKM-BALANCE-YTD > 100.00
               MOVE 100.00 TO PBKM-BALANCE-YTD
           END-IF
           ADD PBKM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBKT-LINE-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-REGION-CD
                   ADD PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E903 CUST ID REQUIRED
      * E998 LINE AMT NEGATIVE
      * E874 LINE AMT OVER 25000
      * E408 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN PBKT-LINE-AMT < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN PBKT-LINE-AMT > 25000.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKM-CARRYFWD-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-CARRYFWD-YTD - (WS-WORK-AMT *
               12)
           COMPUTE PBKT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN OPEN DATE IS PAST
      * PR -> RB WHEN OPEN DATE IS PAST
      * RB -> LS WHEN OPEN DATE IS PAST
      * LS -> CF WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-BALANCE.
           IF PBKM-BALANCE-AMT < 50.00
               MOVE 50.00 TO PBKM-BALANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-AMT > 10000.00
               MOVE 10000.00 TO PBKM-BALANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4230-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD PBKT-CARRYFWD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-CARRYFWD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4240-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINTED YTD.
      *----------------------------------------------------------------*
       4280-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-YTD ROUNDED =
                   PBKM-CARRYFWD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINTED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4300-FLAG-PRINTED.
           IF PBKM-PRINTED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-PRINTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4320-ACCRUE-INTEREST.
           IF PBKM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-YTD * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-UNPOSTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4330-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       4350-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E297 REJECT CD REQUIRED
      * E810 PRINTED YTD NEGATIVE
      * E868 PRINTED YTD OVER 50000
      * E031 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4360-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-REJECT-CD = SPACES
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-YTD < 0
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-YTD > 50000.00
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN PBKM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-CYCLE.
           IF PBKM-CYCLE-CNT < 9999999
               ADD 1 TO PBKM-CYCLE-CNT
           END-IF
           IF PBKM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINTED FEE ON THE INTEREST AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4380-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-PRINTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKT-INTEREST-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO PBKM-PRINTED-AMT
               WHEN PBKT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO PBKM-PRINTED-AMT
               WHEN OTHER
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKT-INTEREST-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-PRINTED-AMT > 0 AND PBKM-PRINTED-AMT < 1.00
               MOVE 1.00 TO PBKM-PRINTED-AMT
           END-IF
           IF PBKM-PRINTED-AMT > 25.00
               MOVE 25.00 TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PBKT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED GRID TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4400-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKT-INTEREST-AMT NOT < WS-UNPOSTED-GRID-LOW(WS-TX)
                   MOVE WS-UNPOSTED-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED FEE ON THE CARRYFWD YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4410-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-UNPOSTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-UNPOSTED-AMT ROUNDED = PBKM-CARRYFWD-YTD
                       * PBKM-FEE-PCT / 200
               WHEN PBKT-TRAN-CD = 'UP'
                   MOVE 5.00 TO PBKM-UNPOSTED-AMT
               WHEN PBKT-TRAN-CD = 'PR'
                   MOVE 15.00 TO PBKM-UNPOSTED-AMT
               WHEN OTHER
                   COMPUTE PBKM-UNPOSTED-AMT ROUNDED = PBKM-CARRYFWD-YTD
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-UNPOSTED-AMT > 0 AND PBKM-UNPOSTED-AMT < 2.00
               MOVE 2.00 TO PBKM-UNPOSTED-AMT
           END-IF
           IF PBKM-UNPOSTED-AMT > 35.00
               MOVE 35.00 TO PBKM-UNPOSTED-AMT
           END-IF
           ADD PBKM-UNPOSTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4420-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4430-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-YTD < 0.00
               MOVE 0.00 TO PBKM-CARRYFWD-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-YTD > 500.00
               MOVE 500.00 TO PBKM-CARRYFWD-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PRINTED YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4440-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PBKM-PRINTED-YTD *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PBKM-PRINTED-YTD *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4450-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE CARRYFWD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4460-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBKM-CARRYFWD-YTD NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4470-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-AMT < 10.00
               MOVE 10.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-AMT > 1000.00
               MOVE 1000.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       4480-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-INTEREST-AMT ROUNDED =
                   PBKM-CARRYFWD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PRINTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       4490-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-INTEREST-AMT ROUNDED =
                   PBKM-PRINTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       4500-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-CARRYFWD-AMT ROUNDED =
                   PBKM-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKT-CARRYFWD-AMT
           END-IF
           ADD PBKT-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4510-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4520-FLAG-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-UNPOSTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4530-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKM-LINE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4540-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 5
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4550-ACCRUE-INTEREST.
           IF PBKM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-AMT * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4560-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4570-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-SOURCE-CD
                   ADD PBKM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4580-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4590-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKT-CARRYFWD-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKT-CARRYFWD-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-BALANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4600-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKM-CARRYFWD-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-CARRYFWD-AMT - (WS-WORK-AMT *
               12)
           COMPUTE PBKT-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4610-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4620-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 6
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE BALANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4630-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PBKM-BALANCE-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4640-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4650-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4660-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4670-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PBKT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-PBDTE.
           INITIALIZE PBDTE-PARMS
           MOVE PBKM-PRINTED-YTD
             TO PBDTE-PRINTED-YTD
           MOVE PBKM-INTEREST-YTD
             TO PBDTE-INTEREST-YTD
           MOVE PBKM-UNPOSTED-YTD
             TO PBDTE-UNPOSTED-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBDTE-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-AMT
             TO PBDTE-CARRYFWD-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBDTE-CARRYFWD-YTD
           MOVE PBKM-NEXT-DUE-DATE
             TO PBDTE-NEXT-DUE-DATE
           MOVE PBKM-MATURITY-DATE
             TO PBDTE-MATURITY-DATE
           MOVE PBKM-OPEN-DATE
             TO PBDTE-OPEN-DATE
           MOVE PBKM-CLOSE-DATE
             TO PBDTE-CLOSE-DATE
           MOVE PBKM-REGION-CD
             TO PBDTE-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBDTE-SOURCE-CD
           MOVE PBKM-BRANCH-CD
             TO PBDTE-BRANCH-CD
           MOVE PBKM-PRINTER-CD
             TO PBDTE-PRINTER-CD
           MOVE PBKM-REVIEW-FLAG
             TO PBDTE-REVIEW-FLAG
           MOVE PBKM-EXEMPT-FLAG
             TO PBDTE-EXEMPT-FLAG
           MOVE PBKM-SUSPEND-FLAG
             TO PBDTE-SUSPEND-FLAG
           MOVE PBKM-CYCLE-CNT
             TO PBDTE-CYCLE-CNT
           MOVE PBKM-RETRY-CNT
             TO PBDTE-RETRY-CNT
           MOVE PBKM-MARGIN
             TO PBDTE-MARGIN
           MOVE PBKM-FEE-PCT
             TO PBDTE-FEE-PCT
           MOVE PBKM-CUST-ID
             TO PBDTE-CUST-ID
           MOVE PBKM-NAME
             TO PBDTE-NAME
           CALL 'PBDTE' USING PBDTE-PARMS
           EVALUATE PBDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBDTE-INTEREST-YTD
                     TO PBKM-INTEREST-YTD
                   MOVE PBDTE-CARRYFWD-AMT
                     TO PBKM-CARRYFWD-AMT
                   MOVE PBDTE-REVIEW-FLAG
                     TO PBKM-REVIEW-FLAG
                   MOVE PBDTE-SUSPEND-FLAG
                     TO PBKM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-PBDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PBKT-KEY TO RD-KEY
           MOVE PBKT-TRAN-CD TO RD-CODE
           MOVE PBKT-PRINTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PBKT-KEY TO RD-KEY
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
           DISPLAY 'PBKPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PBKPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
